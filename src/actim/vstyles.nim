import std/[macros, genasts, sugar, sequtils, strutils, strformat, sets, tables, dom]


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
    result.add genAst(k, funcName = ident($k)) do:
      func funcName*(v: int|float): Size =
        when v is float: Size(kind: k, isFloat: true,  fval: v)
        else:            Size(kind: k, isFloat: false, ival: v)
makeSizeInitFuncs()


macro genSizeMulDiv: untyped =
  result = newStmtList()
  for op in ["*", "/"]:
    let opSym = nnkAccQuoted.newTree(ident(op))
    let isMul = op == "*"
    result.add genAst(opSym, isMul) do:
      when isMul:
        type ftype = float
      else:
        type ftype = int|float

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


macro genSizeAddSub: untyped =
  result = newStmtList()
  for opEnum in [opAdd, opSub]:
    let opSym = nnkAccQuoted.newTree(ident($opEnum))
    result.add genAst(opSym, opEnum) do:

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
  VStyle = object
    attrs: Table[string, string]
    selectors: Table[string, Table[string, string]]

# styles will be collected once before render calls to place just once
# and every render call, additional styles will be collected
var
  styles: seq[VStyle] = @[]
  styleIdBase = 0  # where to start to count when naming class (important for additional styles)


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
    style.selectors[selector] = initTable[string, string]()
  style.selectors[selector][attr] = v

proc className*(id: VStyleId): string =
  fmt"s{ int(id)}"

proc renderStyles*: string =
  for (i, style) in styles.pairs:
    template addDef(attrs: Table[string, string], selector = "") =
      result &=
      "." & className(VStyleId(styleIdBase + i)) & selector & "{" &
      join(collect(
        for (a, v) in attrs.pairs:
          a & ":" & v & ";"
      )) &
      "}\n"
    addDef style.attrs
    for (selector, attrs) in style.selectors.pairs:
      addDef attrs, ":" & selector

proc getStaticStyles*: string =
  result = renderStyles()
  styleIdBase = len(styles)
  styles = @[]

proc getDynamicStyles*: string =
  result = renderStyles()
  styles = @[]

proc `[]`(styles: seq[VStyle], id: VStyleId): VStyle =
  styles[int(id) - styleIdBase]

proc `[]`(styles: var seq[VStyle], id: VStyleId): var VStyle =
  styles[int(id) - styleIdBase]

func `==`(a,b: VStyleId): bool {.borrow.}

macro extendStyle*(extends: VStyleId, body: untyped): VStyleId =
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
        stmnt[0] = showAttrValIdent
        body[i] =
          if selector == "": newCall(bindSym"setAttr", styleVar, attrName, stmnt)
          else:              newCall(bindSym"setAttr", styleVar, newLit(selector), attrName, stmnt)

  transformBody body

  genAst(showAttrValIdent, extends, body, styleVar, result = ident"result"):
    var styleVar =
      when int(extends) >= 0: styles[extends]
      else: VStyle()
    body
    styles &= styleVar
    VStyleId(styleIdBase + high(styles))

template newStyle*(body: untyped): VStyleId =
  extendStyle(VStyleId(-1), body)