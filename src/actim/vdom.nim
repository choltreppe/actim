import std/[sequtils, strutils, strformat, sets, tables, sugar, dom]
import std/[macros, genasts]
import ./vstyles
export tables


type
  EventKind* {.pure.} = enum
    onclick, oncontextmenu, ondblclick, onkeyup, onkeydown, onkeypressed, onfocus, onblur, onchange, nscroll,
    onmousedown, onmouseenter, onmouseleave, onmousemove, onmouseout, onmouseover, onmouseup,
    ondrag, ondragend, ondragenter, ondragleave, ondragover, ondragstart, ondrop,
    onsubmit, oninput,
    onanimationstart, onanimationend, onanimationiteration,
    onkeyupenter, onkeyuplater, onload,
    ontransitioncancel, ontransitionend, ontransitionrun, ontransitionstart,
    onwheel

  EventHandler* = proc(e: Event, n: VNode)

  VNodeKind = enum
    text,

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
    case kind: VNodeKind
    of text: text: string
    else:
      style: seq[VStyleId]
      childs: seq[VNode]
      eventHandlers: Table[EventKind, EventHandler]
    node: Node

const
  selfClosing = {area, br, col, embed, hr, img, input, param, source, track, wbr}


func `$`*(nodes: seq[VNode], ident = 0): string

func `$`*(node: VNode, ident = 0): string =
  let identStr = "  ".repeat(ident)
  case node.kind
  of text:
    identStr & node.text & "\n"
  of selfClosing:
    fmt "{identStr}<{node.kind}>\n"
  else:
    fmt "{identStr}<{node.kind}>\n{`$`(node.childs, ident+1)}{identStr}</{node.kind}>\n"

func `$`*(nodes: seq[VNode], ident = 0): string =
  for node in nodes:
    result &= `$`(node, ident)

func newVNode*(s: string): VNode =
  VNode(kind: text, text: s)

func newVNode*(
  kind: VNodeKind,
  childs: seq[VNode] = @[],
  style: seq[VStyleId] = @[],
  handlers = initTable[EventKind, EventHandler]()
): VNode =
  case kind
  of text: assert false
  else:
    if kind in selfClosing: assert len(childs) == 0
    result = VNode(kind: kind, childs: childs, style: style, eventHandlers: handlers)

var currVDom, prevVDom: seq[VNode]

template collectNodes*(body: untyped): seq[VNode] =
  let start = len(currVDom)
  body
  let nodes = currVDom[start..^1]
  currVDom.setLen start
  nodes

template newVNodeWith*(kind: VNodeKind, body: untyped): VNode =
  block:
    var style {.inject.}: seq[VStyleId]
    var handlers {.inject.}: Table[EventKind, EventHandler]
    newVNode(kind, collectNodes(body), style, handlers)


macro buildVNodeBuildTemplates: untyped =
  result = newStmtList()
  for kind in getType(VNodeKind)[2..^1]:  # skipping text
    result.add genAst(kind = ident(kind.strVal)) do:
      template kind*       = currVDom.add newVNode(kind)
      template kind*(body) = currVDom.add newVNodeWith(kind, body)
buildVNodeBuildTemplates()

proc text*(s: string) =
  currVDom.add newVNode(s)


proc redraw*

proc renderDom*(root: Node) =

  template withNewNode(vn: VNode, body: untyped): untyped =
    case vn.kind
    of text:
      vn.node = document.createTextNode(vn.text.cstring)
      body
    else:
      vn.node = document.createElement(($vn.kind).cstring)
      vn.node.class = vn.style.map(vstyles.className).join(" ")
      for (kind, handler) in vn.eventHandlers.pairs:
        vn.node.addEventListener(
          ($kind)[2..^1].cstring,
          (proc: proc(e: Event) =   # capture handler
            let handler = handler
            let vnode = vn
            return proc(e: Event) =
              handler(e, vnode)
              redraw()
          )()
        )
      body
      update(vn.childs, @[], vn.node)

  proc update(currs, prevs: seq[VNode], parent: Node) =
    let commonLen = min(len(currs), len(prevs))

    for i in 0 ..< commonLen:
      let curr = currs[i]
      let prev = prevs[i]
      if curr.kind != prev.kind:
        debugEcho "diff"
        withNewNode(curr):
          parent.insertBefore(curr.node, prev.node)
          parent.removeChild(prev.node)
      else:
        curr.node = prev.node
        if curr.kind == text:
          if curr.text != prev.text:
            debugEcho "text diff"
            curr.node.nodeValue = curr.text.cstring
        else:
          update(curr.childs, prev.childs, curr.node)

    if len(currs) > commonLen:
      for curr in currs[commonLen..^1]:
        debugEcho "new"
        withNewNode(curr):
          parent.appendChild(curr.node)

    elif len(prevs) > commonLen:
      for prev in prevs[commonLen..^1]:
        debugEcho "del"
        parent.removeChild(prev.node)

  debugEcho "render.."
  update(currVDom, prevVDom, root)

  prevVDom = currVDom
  currVDom = @[]


type Renderer = object
  buildProc: proc()
  rootNode: Node
  styleNode: Node

var renderer: Renderer

proc redraw* =
  renderer.buildProc()
  renderer.styleNode.innerHTML = getDynamicStyles()
  renderDom renderer.rootNode

proc setRenderer*(buildProc: proc(), root: Node = document.body) =
  let staticStyles = document.createElement("style")
  staticStyles.innerHTML = getStaticStyles()
  document.head.appendChild staticStyles

  let dynamicStyles = document.createElement("style")
  document.head.appendChild dynamicStyles
  renderer = Renderer(
    buildProc: buildProc,
    rootNode: root,
    styleNode: dynamicStyles
  )

  redraw()