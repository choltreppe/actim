#[ 
The `vstyles` provides a small DSL to define css styles, that can be applied to `VNode`s.
The `vstyles` are all collected in a global list, and can be added to `VNode`s only by `id`.
To create a vstyle there there are multiple macros/templates (`newVStyle`, `addNewVStyle`, `extendVStyle`, `addExtendVStyle`) that all work with the same DSL for defining styles.
]#
runnableExamples:

  let somePadding = 20.px
  let someOtherPadding = (somePadding * 2) + 3.px
  let someColor = "#44ffaa"

  let someStyleId = newVStyle:
    "height": 10 * somePadding
    "width": 40.pct
    "padding": fmt"{somePadding} {someOtherPadding}"
    "font-weight": "bold"
    ++ ":hover":
      "background-color": someColor

  let someBaseStyle = newVStyle:
    "width": 50.pct

  let someOtherStyleId = extendVStyle someBaseStyle:
    "height": 50.pct


import std/[macros, genasts, sugar, sequtils, strutils, strformat, sets, tables]


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
  VAttrs = OrderedTable[string, string]
  VStyle* = OrderedTable[string, VAttrs]

func newVAttrs: VAttrs = initOrderedTable[string, string]()

func toCssIdent(s: string): string =
  for c in s:
    result.add:
      if c == '_': "-"
      elif c.isUpperASCII: fmt"-{c.toLowerASCII}"
      else: $c

proc setAttr(style: var VStyle, selector,attr,v: string) =
  if selector notin style:
    style[selector] = newVAttrs()
  style[selector][attr.toCssIdent] = v


macro extendVStyle*(extends: VStyle, body: untyped): VStyle =
  ## Extend a vstyle.
  ## uses the same syntax as `newVStyle <#newVStyle.t,untyped>`_

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
        debugEcho stmnt[0].kind
        assert $stmnt[0] == "++"
        stmnt[2].expectKind(nnkStmtList)
        transformBody stmnt[2], selector & stmnt[1].strVal
        body[i] = stmnt[2]

      of nnkForStmt:
        transformBody stmnt[^1], selector
        body[i] = stmnt

      of nnkIfStmt:
        for branch in stmnt: transformBody branch[^1], selector
        body[i] = stmnt

      else:
        stmnt.expectKind(nnkCall)
        stmnt.expectLen(2)
        body[i] = newCall(bindSym"setAttr", styleVar, newLit(selector), stmnt[0], prefix(stmnt[1], "$"))

  transformBody body

  genAst(showAttrValIdent, extends, body, styleVar, result = ident"result"):
    var styleVar = extends
    body
    styleVar

template newVStyle*(body: untyped): VStyle =
  ## Create a vstyle.
  runnableExamples:
    let style = newVStyle:
      "padding": fmt"{5.px} {5.px * 2}"
      "background-color": "#44ffaa"
      "font-weight": "bold"

  extendVStyle(VStyle(), body)

proc merge(a,b: VAttrs): VAttrs =
  result = a
  for (k, v) in b.pairs:
    result[k] = v

proc merge*(a,b: VStyle): VStyle =
  ## Merge 2 style defenitions.
  ## If an attribute is defined in `a` and `b`, the value of `b` is used.
  for k, v in a:
    result[k] =
      if k in b: merge(v, b[k])
      else: v
  for k, v in b:
    if k notin result:
      result[k] = v


type EmptyStyleSelectorError* = ref object of CatchableError

func renderVStyle*(style: VStyle, baseSelector = ""): string =
    for selector, attrs in style:
      if baseSelector == "" and selector == "":
        raise EmptyStyleSelectorError(msg: "trying to render style with empty selector")
      result &= baseSelector & selector & "{"
      for attr, val in attrs:
        result &= attr & ": " & val & "; "
      result &= "}\n"

func pathSelector*(baseId: string, pos: openarray[Natural]): string =
  assert baseId != ""
  result = baseId
  for pos in pos:
    result &= " > *:first-child"
    for _ in 0 ..< pos:
      result &= " + *"