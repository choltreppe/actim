runnableExamples:

  proc buildDom: VNode =
    var testText {.global.} = "foo"

    buildVNode tdiv:
      handle click:
        debugEcho vnode
        testText = "ba"

      ++ text testText

      ++ tdiv:
        ++ text "bla"
        ++ br
        ++ a:
          attr href: "/"
          ++ text "some link"

  setRenderer buildDom


import std/[sequtils, strutils, strformat, sets, tables]
import std/[macros, genasts]
import ./vstyles
export tables

when defined(js):
  import std/dom
  proc tagName*(n: Node): cstring {.importjs: "#.tagName", nodecl.}
else:
  type
    Node = ref object
    Event = ref object


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

func text*(s: varargs[string, `$`]): VNode =
  ## Create a new text vnode
  result = VNode(kind: textnode, text: "")
  for s in s:
    result.text &= s

func newVNode*(
  kind: VNodeKind,
  childs: seq[VNode] = @[],
  style: seq[VStyleId] = @[],
  handlers = initTable[string, EventHandler]()
): VNode =
  ## Create a new vnode
  case kind
  of textnode: assert false
  else:
    if kind in selfClosing: assert len(childs) == 0
    result = VNode(kind: kind, childs: childs, style: style, handlers: handlers)

when defined(js):

  proc redraw*

  macro handle*(kind, body: untyped) =
    ## Add event handler to node
    runnableExamples:
      buildVNode input:
        attr type: "text"
        handle click:
          echo vnode.attributes.value

    kind.expectKind({nnkIdent, nnkSym})
    genAst(vnode = ident"vnode", kind = macros.strVal(kind), body, event = ident"event"):
      vnode.handlers[kind] = proc(event: Event) =
        body
        redraw()

macro style*(id: VStyleId|seq[VStyleId]) =
  ## Add vstyle(s) to node.
  runnableExamples:
    buildVNode tdiv:
      ++ text "foo"
      style: addNewVStyle:
        padding 5.px
        backgroundColor {"#44ffaa"}

  genAst(vnode = ident"vnode", id):
    vnode.style &= id

macro attr*(a,val: untyped) =
  ## Set an attribute of node.
  runnableExamples:
    buildVNode a:
      attr href: "/"
      ++ text "home"

  a.expectKind({nnkIdent, nnkSym})
  genAst(vnode = ident"vnode", s = macros.strVal(a), val):
    vnode.attributes[s] = val

template buildVNode*(vnkind: VNodeKind, body: untyped): VNode =
  ## Build a node with childs/attributes/handlers
  block:
    var vnode {.inject.} = newVNode(vnkind)
    body
    vnode

template extendVNode*(base: VNode, body: untyped): VNode =
  ## Extend an existing node.
  ## Using the same syntax as `buildVNode`
  block:
    var vnode {.inject.} = base
    body
    vnode

#[template buildVNodes*(body: untyped): seq[VNode] =
  block:
    let vnode: VNode = nil   # to prevent access to some outer vnode
    var vnodes {.inject.}: seq[VNode]

    template add(vnkind: VNodeKind) {.inject.} =
      vnodes &= ne

    body
    vnodes]#

template `++`*(vnkind: VNodeKind) {.dirty.} =
  vnode.childs &= newVNode(vnkind)

template `++`*(vnkind: VNodeKind, body: untyped) {.dirty.} =
  vnode.childs &= buildVNode(vnkind, body)

template `++`*(vn: VNode | seq[VNode]) {.dirty.} =
  vnode.childs &= vn

macro `+>`*(head, body: untyped) =
  ## Add child by calling a proc
  let genVnode =
    case head.kind
    of nnkIdent, nnkSym: newCall(head, body)
    of nnkCall, nnkCommand:
      var call = head
      call.add body
      call
    else:
      error "invalid syntax", head  #TODO better msg
      newEmptyNode()
  genAst(vnode=ident"vnode", genVnode):
    vnode.childs &= genVnode


when defined(js):

  type Renderer = object
    case routing: bool
    of true:  buildProcRoute: proc(route: string): VNode
    of false: buildProc:      proc: VNode
    prevDom: VNode

  var renderers: seq[Renderer]

  proc redraw(i: Natural, route: string) =

    proc updateStyleClasses(vnode: VNode, prevStyle: seq[VStyleId] = @[]) =
      if true: #vnode.style != prevStyle:
        vnode.node.class = vnode.style.map(vstyles.className).join(" ")

    proc removeEventListeners(vnode: VNode) =
      if vnode.kind != textnode:
        for (kind, handler) in vnode.handlers.pairs:
          vnode.node.removeEventListener(kind.cstring, handler)

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
        if "value" in vnode.attributes:
          vnode.node.value = vnode.attributes["value"].cstring
        for (kind, handler) in vnode.handlers.pairs:
          vnode.node.addEventListener(kind.cstring, handler)
        update(vnode.childs, @[], vnode.node)


    proc update(curr, prev: VNode, parent: Node) =

      # completly replace node
      if curr.kind != prev.kind:
        newNode(curr)
        parent.insertBefore(curr.node, prev.node)
        removeEventListeners(prev)
        parent.removeChild(prev.node)

      # just update node
      else:
        curr.node = prev.node

        if curr.kind == textnode:
          if curr.text != prev.text:
            curr.node.nodeValue = curr.text.cstring

        else:
          curr.updateStyleClasses(prev.style)

          for a in prev.attributes.keys:
            if a notin curr.attributes:
              curr.node.setAttr(a.cstring, "")
          for (a,v) in curr.attributes.pairs:
            if a notin prev.attributes or prev.attributes[a] != v:
              curr.node.setAttr(a.cstring, v.cstring)

          if "value" in curr.attributes:
            let value = curr.attributes["value"].cstring
            if curr.node.value != value:
              curr.node.value = value

          for (ekind, handler) in prev.handlers.pairs:
            if ekind notin curr.handlers or curr.handlers[ekind] != handler:
              curr.node.removeEventListener(ekind, handler)
              prev.handlers.del(ekind)
          for (ekind, handler) in curr.handlers.pairs:
            if ekind notin prev.handlers:
              curr.node.addEventListener(ekind, handler)

          update(curr.childs, prev.childs, curr.node)

    proc update(currs, prevs: seq[VNode], parent: Node) =
      let commonLen = min(len(currs), len(prevs))

      # update nodes
      for i in 0 ..< commonLen:
        update(currs[i], prevs[i], parent)

      # add new nodes
      if len(currs) > commonLen:
        for curr in currs[commonLen..^1]:
          newNode(curr)
          parent.appendChild(curr.node)

      # remove extra nodes
      elif len(prevs) > commonLen:
        for prev in prevs[commonLen..^1]:
          removeEventListeners(prev)
          parent.removeChild(prev.node)


    let dom =
      if renderers[i].routing: renderers[i].buildProcRoute(route)
      else: renderers[i].buildProc()
    update(dom, renderers[i].prevDom, renderers[i].prevDom.node.parentNode)
    renderers[i].prevDom = dom

    renderStyles()


  proc getHashPart: string =
    result = $window.location.hash
    if len(result) > 0:
      assert result[0] == '#'
      result = result[1..^1]

  proc redraw(i: int) =
    redraw(i, getHashPart())

  proc redraw =
    let route = getHashPart()
    for i in 0 ..< len(renderers):
      redraw(i, route)


  proc setRenderer*(buildProc: proc: VNode, root: Node = document.getElementById("ROOT")) =
    renderers &= Renderer(
      routing: false,
      buildProc: buildProc,
      prevDom: VNode(
        kind: parseEnum[VNodeKind](($root.tagName).toLowerAscii),
        node: root
      )
    )
    initVStyles()
    redraw()

  proc setRenderer*(buildProc: proc(route: string): VNode, root: Node = document.getElementById("ROOT")) =
    renderers &= Renderer(
      routing: true,
      buildProcRoute: buildProc,
      prevDom: VNode(
        kind: parseEnum[VNodeKind](($root.tagName).toLowerAscii),
        node: root
      )
    )
    let i = high(renderers)
    window.addEventListener("hashchange") do (e: Event):
      redraw(i)
      
    initVStyles()
    redraw()