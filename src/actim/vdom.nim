import std/[sequtils, strutils, strformat, sets, tables, sugar, dom]
import std/[macros, genasts]
import ./vstyles
export tables


type
  EventHandler* = proc(e: Event)

  VNodeKind* = enum
    textnode,

    section, nav, article, aside,
    h1, h2, h3, h4, h5, h6, hgroup,
    header, footer, address, main,

    p, hr, pre, blockquote, ol, ul, li,
    dl, dt, dd,
    figure, figcaption,

    tdiv = "div",

    a, em, strong, small,
    strikethrough = "s", cite, quote,
    dfn, abbr, data, time, code, `var` = "var", samp,
    kdb, sub, sup, italic = "i", bold = "b", underlined = "u",
    mark, ruby, rt, rp, bdi, dbo, span, br, wbr,
    ins, del, img, iframe, embed, `object` = "object",
    param, video, audio, source, track, canvas, map, area,

    # SVG elements, see: https://www.w3.org/TR/SVG2/eltindex.html
    animate, animateMotion, animateTransform, circle, clipPath, defs, desc,
    `discard` = "discard", ellipse, feBlend, feColorMatrix, feComponentTransfer,
    feComposite, feConvolveMatrix, feDiffuseLighting, feDisplacementMap,
    feDistantLight, feDropShadow, feFlood, feFuncA, feFuncB, feFuncG, feFuncR,
    feGaussianBlur, feImage, feMerge, feMergeNode, feMorphology, feOffset,
    fePointLight, feSpecularLighting, feSpotLight, feTile, feTurbulence,
    filter, foreignObject, g, image, line, linearGradient, marker, mask,
    metadata, mpath, path, pattern, polygon, polyline, radialGradient, rect,
    `set` = "set", stop, svg, switch, symbol, stext = "text", textPath, tspan,
    unknown, use, view,

    maction, math, menclose, merror, mfenced, mfrac, mglyph, mi, mlabeledtr,
    mmultiscripts, mn, mo, mover, mpadded, mphantom, mroot, mrow, ms, mspace,
    msqrt, mstyle, msub, msubsup, msup, mtable, mtd, mtext, mtr, munder,
    munderover, semantics,

    table, caption, colgroup, col, tbody, thead,
    tfoot, tr, td, th,

    form, fieldset, legend, label, input, button,
    select, datalist, optgroup, option, textarea,
    keygen, output, progress, meter,
    details, summary, command, menu

  VNode* = ref object
    case kind*: VNodeKind
    of textnode: text*: string
    else:
      style*: seq[VStyleId]
      attributes*: Table[string, string]
      childs*: seq[VNode]
      handlers*: Table[string, EventHandler]
    node*: Node

const
  selfClosing = {area, br, col, embed, hr, img, input, param, source, track, wbr}


func `$`*(nodes: seq[VNode], ident = 0): string

func `$`*(node: VNode, ident = 0): string =
  let identStr = "  ".repeat(ident)
  case node.kind
  of textnode:
    identStr & node.text & "\n"
  of selfClosing:
    fmt "{identStr}<{node.kind}>\n"
  else:
    fmt "{identStr}<{node.kind}>\n{`$`(node.childs, ident+1)}{identStr}</{node.kind}>\n"

func `$`*(nodes: seq[VNode], ident = 0): string =
  for node in nodes:
    result &= `$`(node, ident)

func newVNode*(s: string): VNode =
  VNode(kind: textnode, text: s)

func newVNode*(
  kind: VNodeKind,
  childs: seq[VNode] = @[],
  style: seq[VStyleId] = @[],
  handlers = initTable[string, EventHandler]()
): VNode =
  case kind
  of textnode: assert false
  else:
    if kind in selfClosing: assert len(childs) == 0
    result = VNode(kind: kind, childs: childs, style: style, handlers: handlers)

var currVDom, prevVDom: seq[VNode]

template collectVNodes*(body: untyped): seq[VNode] =
  let start = len(currVDom)
  body
  when not defined(release):
    for i in start ..< len(currVDom) - 1:
      assert currVDom[i].kind != textnode or currVDom[i+1].kind != textnode
  let nodes = currVDom[start..^1]
  currVDom.setLen start
  nodes

proc redraw*

template newVNodeWith*(vnkind: VNodeKind, body: untyped): VNode =
  block:
    var node {.inject.} = newVNode(vnkind)

    template style(id: VStyleId) =
      node.style &= id

    macro attr(a,val: untyped) =
      a.expectKind({nnkIdent, nnkSym})
      genAst(s = macros.strVal(a), val):
        node.attributes[s] = val

    macro handle(ekind, ebody: untyped) =
      ekind.expectKind({nnkIdent, nnkSym})
      let ekind = macros.strVal(ekind)
      let event = ident"event"
      genAst(ekind, ebody, event):
        node.handlers[ekind] = proc(event: Event) =
          ebody
          redraw()

    node.childs = collectVNodes(body)
    node

proc addVNode*(node: VNode) =
  currVDom.add node

proc vn*(kind: VNodeKind) =
  addVNode newVNode(kind)

template vn*(kind: VNodeKind, body: untyped) =
  addVNode newVNodeWith(kind, body)


#[macro buildVNodeBuildTemplates: untyped =
  result = newStmtList()
  for kindSym in getType(VNodeKind)[2..^1]:  # skipping text
    let kindStr = kindSym.strVal
    let procName = ident("new" & (
        if kindStr == "tdiv": "div"
        else: kindStr
      ))
    result.add genAst(kind = ident(kindStr), procName) do:
      template procName*       = add(kind)
      template procName*(body) = add(kind, body)
buildVNodeBuildTemplates()]#

proc text*(s: string) =
  addVNode newVNode(s)


proc renderDom*(root: Node) =

  proc updateStyleClasses(vnode: VNode, prevStyle: seq[VStyleId] = @[]) =
    if vnode.style != prevStyle:
      vnode.node.class = vnode.style.map(vstyles.className).join(" ")

  proc removeEventListeners(vnode: VNode) =
    if vnode.kind != textnode:
      for (kind, handler) in vnode.handlers.pairs:
        debugEcho "remove listener"
        vnode.node.removeEventListener(kind.cstring, handler)
      #clear vnode.handlers

  proc update(currs, prevs: seq[VNode], parent: Node)

  proc newNode(vnode: VNode) =
    case vnode.kind
    of textnode:
      vnode.node = document.createTextNode(vnode.text.cstring)
    else:
      vnode.node = document.createElement(($vnode.kind).cstring)
      vnode.updateStyleClasses()
      for (a,v) in vnode.attributes.pairs:
        vnode.node.setAttr(a.cstring, v.cstring)
      for (kind, handler) in vnode.handlers.pairs:
        debugEcho "add listener"
        vnode.node.addEventListener(kind.cstring, handler)
      update(vnode.childs, @[], vnode.node)


  proc update(currs, prevs: seq[VNode], parent: Node) =
    let commonLen = min(len(currs), len(prevs))

    for i in 0 ..< commonLen:
      let curr = currs[i]
      let prev = prevs[i]

      # completly replace node
      if curr.kind != prev.kind:
        debugEcho "diff"
        newNode(curr)
        parent.insertBefore(curr.node, prev.node)
        removeEventListeners(prev)
        parent.removeChild(prev.node)

      # just update node
      else:
        curr.node = prev.node

        if curr.kind == textnode:
          if curr.text != prev.text:
            debugEcho "text diff"
            curr.node.nodeValue = curr.text.cstring

        else:
          curr.updateStyleClasses(prev.style)

          for a in prev.attributes.keys:
            if a notin curr.attributes:
              curr.node.setAttr(a.cstring, "")
          for (a,v) in curr.attributes.pairs:
            if a notin prev.attributes or prev.attributes[a] != v:
              curr.node.setAttr(a.cstring, v.cstring)

          for (ekind, handler) in prev.handlers.pairs:
            if ekind notin curr.handlers or curr.handlers[ekind] != handler:
              curr.node.removeEventListener(ekind, handler)
              prev.handlers.del(ekind)
          for (ekind, handler) in curr.handlers.pairs:
            if ekind notin prev.handlers:
              curr.node.addEventListener(ekind, handler)

          update(curr.childs, prev.childs, curr.node)

    # add new nodes
    if len(currs) > commonLen:
      for curr in currs[commonLen..^1]:
        debugEcho "new"
        newNode(curr)
        parent.appendChild(curr.node)

    # remove extra nodes
    elif len(prevs) > commonLen:
      for prev in prevs[commonLen..^1]:
        debugEcho "del"
        removeEventListeners(prev)
        parent.removeChild(prev.node)

  debugEcho "render.."
  update(currVDom, prevVDom, root)

  prevVDom = currVDom
  currVDom = @[]


type Renderer = object
  buildProc: proc()
  rootNode: Node

var renderer: Renderer

proc redraw* =
  renderer.buildProc()
  renderStyles()
  renderDom renderer.rootNode

proc setRenderer*(buildProc: proc(), root: Node = document.body) =
  renderer = Renderer(
    buildProc: buildProc,
    rootNode: root,
  )
  initVStyles()
  redraw()