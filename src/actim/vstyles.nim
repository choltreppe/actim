#[ 
The `vstyles` provides a small DSL to define css styles, that can be applied to `VNode`s.
The `vstyles` are all collected in a global list, and can be added to `VNode`s only by `id`.
To create a vstyle there there are multiple macros/templates (`newVStyle`, `addNewVStyle`, `extendVStyle`, `addExtendVStyle`) that all work with the same DSL for defining styles.
]#
runnableExamples:

  let somePadding = 20.px
  let someOtherPadding = (somePadding * 2) + 3.px
  let someColor = "#44ffaa"

  let someStyleId = addNewVStyle:
    height {10 * somePadding}
    width {40.pct}
    padding {somePadding}, {someOtherPadding}
    fontWeight bold
    @:hover:
      backgroundColor {someColor}

  let someBaseStyle = newVStyle:
    width {50.pct}

  let someOtherStyleId = addExtendVStyle someBaseStyle:
    height {50.pct}


import std/[macros, genasts, sugar, sequtils, strutils, strformat, sets, tables]

when defined(js):
  import std/dom
else:
  type Node = ref object


type
  Op = enum opAdd="+", opSub="-"
  SizeKind = enum px, em, pct, vw, vh, vmin, vmax, calc
  Size* = ref object
    case kind: SizeKind
    of calc:
      op: Op
      lhs,rhs: Size
    else:
      case isFloat: bool
      of true:  fval: float
      of false: ival: int

func `$`*(s: Size): string =
  func showCalc(s: Size): string =
    if s.kind == calc:
      "(" & showCalc(s.lhs) & " " & $s.op & " " & showCalc(s.rhs) & ")"
    else: $s
  if s.kind == calc: "calc" & showCalc(s)
  else:
    ( if s.isFloat: $s.fval
      else:         $s.ival
    ) &
    ( if s.kind == pct: "%"
      else: $s.kind )

macro makeSizeInitFuncs: untyped =
  result = newStmtList()
  for k in px ..< calc:
    result.add genAst(k, v = ident"v", funcName = ident($k)) do:
      func funcName*(v: int|float): Size =
        when v is float: Size(kind: k, isFloat: true,  fval: v)
        else:            Size(kind: k, isFloat: false, ival: v)
makeSizeInitFuncs()


macro genSizeMulDiv: untyped =
  result = newStmtList()
  for op in ["*", "/"]:
    let opSym = nnkAccQuoted.newTree(ident(op))
    let ftype =
      if op == "*": genAst(float)
      else:         genAst(int|float)

    result.add genAst(opSym, s = ident"s", f = ident"f", ftype) do:
      func opSym*(s: Size, f: ftype): Size =
        if s.kind == calc:
          Size(kind: calc, op: s.op, lhs: opSym(s.lhs, f), rhs: opSym(s.rhs,f))
        else:
          Size(
            kind: s.kind,
            isFloat: true,
            fval: opSym( 
                    if s.isFloat: s.fval
                    else:   float(s.ival),
                    float(f)
                  )
          )

      template opSym*(f: int|float, s: Size): Size = opSym(s, f)

genSizeMulDiv()

func `*`*(s: Size, f: int): Size =
  if s.kind == calc:
    Size(kind: calc, op: s.op, lhs: s.lhs*f, rhs: s.rhs*f)
  else:
    if s.isFloat:
      Size(
        kind: s.kind,
        isFloat: true,
        fval: s.fval * float(f)
      )
    else:
      Size(
        kind: s.kind,
        isFloat: false,
        ival: s.ival * f
      )


macro genSizeAddSub =
  result = newStmtList()
  for opEnum in [opAdd, opSub]:
    let opSym = nnkAccQuoted.newTree(ident($opEnum))
    result.add genAst(opSym, opEnum, a = ident"a", b = ident"b") do:

      proc opSym*(a,b: Size): Size =
        if a.kind == b.kind and a.kind != calc:
          if not(a.isFloat) and not(b.isFloat):
            Size(
              kind: a.kind,
              isFloat: false,
              ival: opSym(a.ival, b.ival)
            )
          else:
            Size(
              kind: a.kind,
              isFloat: true,
              fval:
                if   not a.isFloat: opSym(float(a.ival),       b.fval )
                elif not b.isFloat: opSym(      a.fval , float(b.ival))
                else:               opSym(      a.fval ,       b.fval )
            )
        else:
          Size(kind: calc, op: opEnum, lhs: a, rhs: b)

genSizeAddSub()

template `-`*(s: Size): Size = s * -1


type
  VStyleId* = distinct int
  VAttrs = OrderedTable[string, string]
  VStyle* = object
    attrs*: VAttrs
    selectors*: Table[string, VAttrs]
    node: Node

var
  currVStyles, prevVStyles: seq[VStyle]
  vStylesBaseLen = 0  # length the vstyles stack has before render calls

proc initVStyles* =
  ## Initialise vstyles
  ## **Note**: This is called automatically by the `setRenderer` proc.
  vStylesBaseLen = len(currVStyles)

func newVAttrs: VAttrs = initOrderedTable[string, string]()

#proc `[]`(styles: seq[VStyle], id: VStyleId): VStyle =
#  styles[int(id)]

proc `[]`(styles: var seq[VStyle], id: VStyleId): var VStyle =
  styles[int(id)]

func `==`*(a,b: VStyleId): bool {.borrow.}

func `==`*(a,b: VStyle): bool =
  a.attrs == b.attrs and a.selectors == b.selectors

func toCssIdent(s: string): string =
  for c in s:
    result.add:
      if c == '_': "-"
      elif c.isUpperASCII: fmt"-{c.toLowerASCII}"
      else: $c

proc setAttr(style: var VStyle, attr,v: string) =
  style.attrs[attr.toCssIdent] = v

proc setAttr(style: var VStyle, selector,attr,v: string) =
  if selector notin style.selectors:
    style.selectors[selector] = newVAttrs()
  style.selectors[selector][attr.toCssIdent] = v

proc className*(id: VStyleId): string =
  ## **Note**: The `vdom` module uses this for rendering, you shouldn't need to call this yourself in most cases.
  fmt"s{int(id)}"


proc addVStyle*(style: VStyle): VStyleId =
  ## Add a vstyle to the list of vstyles, to be able to apply it to elements
  runnableExamples:
    let style = newVStyle:
      fontWeight bold

    let styleId = addVStyle(style)

  currVStyles &= style
  VStyleId(high(currVStyles))

proc getVStyle*(id: VStyleId): VStyle =
  ## Get the style definition from global vstyle list.
  currVStyles[id]

proc mgetVStyle*(id: VStyleId): var VStyle =
  ## Get a mutable style definition from global vstyle list.
  currVStyles[id]


macro extendVStyle*(extends: VStyle, body: untyped): VStyle =
  ## Extend a vstyle.

  body.expectKind(nnkStmtList)
  var body = body
  while body[0].kind == nnkStmtList: body = body[0]
  
  func showAttrVal(vals: varargs[string, `$`]): string =
    for i, v in vals:
      if i > 0: result &= " "
      result &= v

  let 
    styleVar = genSym(nskVar, "style")
    showAttrValIdent = bindSym("showAttrVal")

  proc transformBody(body: NimNode, selector = "") =
    for (i, stmnt) in body.pairs:
      case stmnt.kind
      of nnkPrefix:
        stmnt.expectLen(3)
        assert stmnt[0].strVal == "@:"
        stmnt[2].expectKind(nnkStmtList)
        transformBody stmnt[2], stmnt[1].strVal
        body[i] = stmnt[2]
      of nnkForStmt:
        transformBody stmnt[^1], selector
        body[i] = stmnt
      of nnkIfStmt:
        for branch in stmnt: transformBody branch[^1], selector
        body[i] = stmnt
      else:
        stmnt.expectKind({nnkCall, nnkCommand})
        let attrName = if stmnt[0].kind == nnkIdent: newLit(stmnt[0].strVal)
                       else: stmnt[0]
        var showAttrCall = newCall(showAttrValIdent)
        for arg in stmnt[1..^1]:
          showAttrCall.add:
            case arg.kind
            of nnkIdent, nnkSym: newLit(toCssIdent(arg.strVal))
            of nnkStrLit: newLit("\"" & arg.strVal & "\"")
            of nnkCurly:
              arg.expectLen(1)
              arg[0]
            else: arg 
        body[i] =
          if selector == "": newCall(bindSym"setAttr", styleVar, attrName, showAttrCall)
          else:              newCall(bindSym"setAttr", styleVar, newLit(selector), attrName, showAttrCall)

  transformBody body

  genAst(showAttrValIdent, extends, body, styleVar, result = ident"result"):
    var styleVar = extends
    body
    styleVar

template extendVStyle*(extendsId: VStyleId, body: untyped): VStyle =
  ## Extend a vstyle.
  ## **Note**: This template is mostly usefull for styles that serve soley as base classes. If you want to use the style on elements you probably want to use `addExtendVStyle <#addExtendVStyle>`_
  extendVStyle(getVStyle(extendsId), body)

template newVStyle*(body: untyped): VStyle =
  ## Create a vstyle.
  ## **Note**: This template is mostly usefull for styles that serve soley as base classes. If you want to use the style on elements you probably want to use `addExtendVStyle <#addExtendVStyle>`_
  runnableExamples:
    let style = newVStyle:
      padding {5.px}, {5.px * 2}
      backgroundColor {"#44ffaa"}
      fontWeight bold

  extendVStyle(VStyle(), body)

template addExtendVStyle*(extends: VStyle|VStyleId, body: untyped): VStyleId =
  ## Extend a vstyle and add it to the list
  addVStyle(extendVStyle(extends, body))

template addNewVStyle*(body: untyped): VStyleId =
  ## Create a vstyle and add it to the list
  addVStyle(newVStyle(body))

proc merge(a,b: VAttrs): VAttrs =
  result = a
  for (k, v) in b.pairs:
    result[k] = v

proc merge*(a,b: VStyle): VStyle =
  ## Merge 2 style defenitions.
  ## If an attribute is defined in `a` and `b`, the value of `b` is used.
  result.attrs = merge(a.attrs, b.attrs)
  for (k, v) in a.selectors.pairs:
    result.selectors[k] =
      if k in b.selectors: merge(v, b.selectors[k])
      else: v
  for (k, v) in b.selectors.pairs:
    if k notin result.selectors:
      result.selectors[k] = v

proc merge*(a,b: VStyleId): VStyle =
  merge(getVStyle(a), getVStyle(b))

proc addMerge*[T: VStyle|VStyleId](a,b: T): VStyleId =
  ## Merge 2 vstyles and add them to the vstyle list.
  addVStyle(merge(a, b))


when defined(js):

  proc renderStyles* =
    ## Update style defenitions in the DOM.
    ## **Note**: The `vdom` module uses this for rendering, you shouldn't need to call this yourself in most cases.

    proc setStyle(i: int) =
      proc addDef(attrs: VAttrs, selector = "") =
        currVStyles[i].node.innerHTML &=
        "." & className(VStyleId(i)) & selector & "{" &
        join(collect(
          for (a, v) in attrs.pairs:
            a & ":" & v & ";"
        )) &
        "}\n"
      currVStyles[i].node.innerHTML = ""
      addDef currVStyles[i].attrs
      for (selector, attrs) in currVStyles[i].selectors.pairs:
        addDef attrs, ":" & selector
    
    proc updateStyle(i: int) =
      if currVStyles[i] != prevVStyles[i]: setStyle(i)

    proc newStyle(i: int) =
      currVStyles[i].node = document.createElement("style")
      setStyle(i)
      document.head.appendChild(currVStyles[i].node)

    proc removeStyle(i: int) =
      document.head.removeChild(prevVStyles[i].node)
      prevVStyles[i].node = nil

    if len(prevVStyles) == 0:
      for i in 0 ..< vStylesBaseLen: newStyle(i)
    else:
      for i in 0 ..< vStylesBaseLen: updateStyle(i)

    let commonLen = min(len(currVStyles), len(prevVStyles))

    for i in vStylesBaseLen ..< commonLen:
      let curr = currVStyles[i]
      let prev = prevVStyles[i]
      currVStyles[i].node = prev.node
      updateStyle(i)

    if len(currVStyles) > commonLen:
      for i in commonLen ..< len(currVStyles):
        newStyle(i)

    if len(prevVStyles) > commonLen:
      for i in commonLen ..< len(prevVStyles):
        removeStyle(i)

    prevVStyles = currVStyles
    currVStyles.setLen vStylesBaseLen