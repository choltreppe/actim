runnableExamples:

  proc buildDom: VNode =
    var testText {.global.} = "foo"

    buildVNode "div":
      handle "click":
        debugEcho vnode
        testText = "ba"

      ++ text testText

      ++ "div":
        ++ text "bla"
        ++ "br"
        ++ "a":
          attr "href": "/"
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
    Node* = ref object
    Event* = ref object


type
  EventHandler* = proc(e: Event)

  VNode* = ref object
    case isText*: bool
    of true: text*: string
    else:
      tag*: string
      styles*: seq[VStyle]
      attributes*: Table[string, string]
      childs*: seq[VNode]
      handlers*: Table[string, EventHandler]


func `$`*(nodes: seq[VNode], ident = 0): string

func `$`*(node: VNode, ident = 0): string =
  let identStr = "  ".repeat(ident)
  case node.isText
  of true:
    result = identStr & node.text & "\n"
  else:
    var attrStr = ""
    for attr, val in node.attributes:
      attrStr &= fmt " {attr}=\"{val}\""
    result = fmt "{identStr}<{node.tag}{attrStr}>\n"
    if len(node.childs) > 0:
      result &= fmt "{`$`(node.childs, ident+1)}{identStr}</{node.tag}>\n"

func `$`*(nodes: seq[VNode], ident = 0): string =
  for node in nodes:
    result &= `$`(node, ident)

func text*(s: varargs[string, `$`]): VNode =
  ## Create a new text vnode
  result = VNode(isText: true, text: "")
  for s in s:
    result.text &= s

func newVNode*(tag: string): VNode =
  ## Create a new vnode
  result = VNode(isText: false, tag: tag)

when defined(js):

  proc redraw*

  macro handle*(kind: string, body: untyped) =
    ## Add event handler to node
    runnableExamples:
      buildVNode input:
        attr "type": "text"
        handle "click":
          echo vnode.attributes.value

    genAst(vnode = ident"vnode", kind, body, event = ident"event"):
      vnode.handlers[kind] = proc(event: Event) =
        #preventDefault event
        body
        redraw()

macro style*(vstyle: VStyle | seq[VStyle]) =
  ## Add vstyle(s) to node.
  runnableExamples:
    buildVNode "div":
      ++ text "foo"
      style: newVStyle:
        "padding": 5.px
        "background-color": "#44ffaa"

  genAst(vnode = ident"vnode", vstyle):
    vnode.styles &= vstyle

macro attr*(a,val: untyped) =
  ## Set an attribute of node.
  runnableExamples:
    buildVNode "a":
      attr "href": "/"
      ++ text "home"

  genAst(vnode = ident"vnode", a, val):
    vnode.attributes[a] = val

template buildVNode*(tag: string, body: untyped): VNode =
  ## Build a node with childs/attributes/handlers
  block:
    var vnode {.inject.} = newVNode(tag)
    body
    vnode

template extendVNode*(base: VNode, body: untyped): VNode =
  ## Extend an existing node.
  ## Using the same syntax as `buildVNode`
  block:
    var vnode {.inject.} = base
    body
    vnode

template buildVNodes*(body: untyped): seq[VNode] =
  runnableExamples:
    buildVNodes:
      ++ "b":
        ++ text "foo"
      ++ text "ba"

  block:
    macro handle(kind, hbody: untyped) {.inject, error.} = discard
    macro style(id: VStyle | seq[VStyle]) {.inject, error.} = discard
    macro attr(a,val: untyped) {.inject, error.} = discard

    var vnode {.inject.} = VNode()
    body
    vnode.childs

template `++`*(vnExpr: VNode) {.dirty.} =
  block:
    let vn = vnExpr
    if vn == nil: raise Exception.newException("node is nil")
    if vn.isText and len(vnode.childs) > 0 and vnode.childs[^1].isText:
      vnode.childs[^1].text &= vn.text
    else:
      vnode.childs &= vn

template `++`*(vns: seq[VNode]) {.dirty.} =
  for vn in vns:
    ++ vn

template `++`*(vn: VNode, body: untyped) {.dirty.} =
  ++ (extendVNode(vn, body))

template `++`*(tag: string) {.dirty.} =
  ++ newVNode(tag)

template `++`*(tag: string, body: untyped) {.dirty.} =
  ++ buildVNode(tag, body)

macro `+>`*(head, body: untyped) =
  ## Add child by calling a proc
  runnableExamples:
    template drawInfoBox(title: string, body: untyped): VNode =
      buildVNode "div":
        attr "class": "info-box"
        ++ "div":
          attr "class": "title"
          ++ text title
        body

    buildVNode "div":
      +> drawInfoBox "foo":
        ++ text "lorem ipsum"

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


var
  globalStyles: seq[VStyle]
  rendering = false

proc globalStyle*(vstyle: VStyle) =
  if rendering: raise Exception.newException("cant set a global style inside render proc")
  globalStyles &= vstyle


when defined(js):

  type Renderer = object
    case routing: bool
    of true:  buildProcRoute: proc(route: string): VNode
    of false: buildProc:      proc: VNode
    postRenderProc: proc()
    prevDom: VNode
    rootNode: Node
    styleNode: Node

  var renderers: seq[Renderer]

  proc redraw(i: Natural, route: string) =
    var
      id = 0
      styleIdLookup = initTable[seq[VStyle], int]()  # for not double defining styles
      css = ""

    proc addStyles(vnode: VNode) =
      if len(vnode.styles) > 0:
        var className = "actim-"
        if vnode.styles in styleIdLookup:
          className &= $styleIdLookup[vnode.styles]
        else:
          className &= $id
          styleIdLookup[vnode.styles] = id
          for style in vnode.styles:
            css &= renderVStyle(style, "."&className).cstring
          inc id

        vnode.attributes.mgetOrPut("class", "") &= " " & className

    proc removeEventListeners(vnode: VNode, node: Node) =
      if not vnode.isText:
        for ekind, handler in vnode.handlers:
          node.removeEventListener(ekind.cstring, handler)

    proc update(currs, prevs: seq[VNode], nodes: seq[Node], parent: Node)

    proc newNode(vnode: VNode): Node =
      if vnode.isText:
        result = document.createTextNode(vnode.text.cstring)

      else:
        result = document.createElement(vnode.tag.cstring)
        
        addStyles(vnode)

        for attr, val in vnode.attributes:
          result.setAttr(attr.cstring, val.cstring)

        if "value" in vnode.attributes:
          result.value = vnode.attributes["value"].cstring

        for ekind, handler in vnode.handlers:
          result.addEventListener(ekind.cstring, handler)

        update(vnode.childs, @[], @[], result)


    proc update(curr, prev: VNode, node: Node, parent: Node) =

      # completly replace node
      if (curr.isText xor prev.isText) or ((not curr.isText) and curr.tag != prev.tag):
        parent.insertBefore(newNode(curr), node)
        removeEventListeners(prev, node)
        parent.removeChild(node)

      # just update node
      else:
        if curr.isText:
          if curr.text != prev.text:
            node.nodeValue = curr.text.cstring

        else:
          addStyles(curr)

          for a in prev.attributes.keys:
            if a notin curr.attributes:
              node.setAttr(a.cstring, "")
          for (a,v) in curr.attributes.pairs:
            if a notin prev.attributes or prev.attributes[a] != v:
              node.setAttr(a.cstring, v.cstring)

          let value =
            if "value" in curr.attributes: curr.attributes["value"].cstring
            else: ""
          if node.value != value:
            node.value = value

          for (ekind, handler) in prev.handlers.pairs:
            if ekind notin curr.handlers or curr.handlers[ekind] != handler:
              node.removeEventListener(ekind, handler)
              prev.handlers.del(ekind)
          for (ekind, handler) in curr.handlers.pairs:
            if ekind notin prev.handlers:
              node.addEventListener(ekind, handler)

          update(curr.childs, prev.childs, node.childNodes, node)

    proc update(currs, prevs: seq[VNode], nodes: seq[Node], parent: Node) =
      assert len(prevs) == len(nodes)

      let commonLen = min(len(currs), len(prevs))

      # update nodes
      for i in 0 ..< commonLen:
        update(currs[i], prevs[i], nodes[i], parent)

      # add new nodes
      if len(currs) > commonLen:
        for i in commonLen ..< len(currs):
          parent.appendChild(newNode(currs[i]))

      # remove extra nodes
      elif len(prevs) > commonLen:
        for _ in commonLen ..< len(prevs):
          parent.removeChild(nodes[commonLen])


    for style in globalStyles:
      css &= renderVStyle(style)

    rendering = true
    let dom =
      if renderers[i].routing: renderers[i].buildProcRoute(route)
      else: renderers[i].buildProc()
    rendering = false

    update(dom, renderers[i].prevDom, renderers[i].rootNode, renderers[i].rootNode.parentNode)
    renderers[i].styleNode.innerHtml = css
    renderers[i].prevDom = dom

    if renderers[i].postRenderProc != nil:
      renderers[i].postRenderProc()


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


  proc newRenderer(rootId: string, postRenderProc: proc()): Renderer =
    result = Renderer(
      prevDom: VNode(isText: false),
      rootNode: document.getElementById(rootId),
      styleNode: document.createElement("style"),
      postRenderProc: postRenderProc
    )
    result.prevDom.tag = ($result.rootNode.tagName).toLowerAscii
    document.head.appendChild(result.styleNode)

  proc setRenderer*(buildProc: proc: VNode, rootId = "ROOT", postRenderProc: proc() = nil) =
    renderers &= newRenderer(rootId, postRenderProc)
    renderers[^1].routing = false
    renderers[^1].buildProc = buildProc
    redraw()

  proc setRenderer*(buildProc: proc(route: string): VNode, rootId = "ROOT", postRenderProc: proc() = nil) =
    renderers &= newRenderer(rootId, postRenderProc)
    renderers[^1].routing = true
    renderers[^1].buildProcRoute = buildProc
    let i = high(renderers)
    window.addEventListener("hashchange") do (e: Event):
      redraw(i)
    redraw()