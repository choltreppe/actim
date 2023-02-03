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
      stylesNode: Node
      attributes*: Table[string, string]
      childs*: seq[VNode]
      handlers*: Table[string, EventHandler]
    node*: Node


func `$`*(nodes: seq[VNode], ident = 0): string

func `$`*(node: VNode, ident = 0): string =
  let identStr = "  ".repeat(ident)
  case node.isText
  of true:
    identStr & node.text & "\n"
  else:
    if len(node.childs) == 0:
      fmt "{identStr}<{node.tag}>\n"
    else:
      fmt "{identStr}<{node.tag}>\n{`$`(node.childs, ident+1)}{identStr}</{node.tag}>\n"

func `$`*(nodes: seq[VNode], ident = 0): string =
  for node in nodes:
    result &= `$`(node, ident)

func text*(s: varargs[string, `$`]): VNode =
  ## Create a new text vnode
  result = VNode(isText: true, text: "")
  for s in s:
    result.text &= s

func newVNode*(
  tag: string,
  childs: seq[VNode] = @[],
  styles: seq[VStyle] = @[],
  handlers = initTable[string, EventHandler]()
): VNode =
  ## Create a new vnode
  result = VNode(isText: false, tag: tag, childs: childs, styles: styles, handlers: handlers)

when defined(js):

  proc redraw*

  macro handle*(kind, body: untyped) =
    ## Add event handler to node
    runnableExamples:
      buildVNode input:
        attr "type": "text"
        handle "click":
          echo vnode.attributes.value

    genAst(vnode = ident"vnode", kind = macros.strVal(kind), body, event = ident"event"):
      vnode.handlers[kind] = proc(e: Event) =
        preventDefault e
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

template style*(body: untyped) =
  style newVStyle(body)

macro attr*(a,val: untyped) =
  ## Set an attribute of node.
  runnableExamples:
    buildVNode "a":
      attr "href": "/"
      ++ text "home"

  genAst(vnode = ident"vnode", s = macros.strVal(a), val):
    vnode.attributes[s] = val

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

template `++`*(vn: VNode | seq[VNode]) {.dirty.} =
  vnode.childs &= vn

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


when defined(js):

  type Renderer = object
    case routing: bool
    of true:  buildProcRoute: proc(route: string): VNode
    of false: buildProc:      proc: VNode
    prevDom: VNode
    rootId: string

  var renderers: seq[Renderer]

  type ChangeRootElementIdError* = ref object of CatchableError

  proc redraw(i: Natural, route: string) =

    let rootId = renderers[i].rootId

    type NodeId = seq[Natural]

    proc updateStyles(vnode: VNode, prevStyles: seq[VStyle], id: NodeId) =
      if len(vnode.styles) > 0 and vnode.styles != prevStyles:
        if vnode.stylesNode == nil:
          vnode.stylesNode = document.createElement("style")
          document.head.appendChild(vnode.stylesNode)
        vnode.stylesNode.innerHtml = ""
        for style in vnode.styles:
          vnode.stylesNode.innerHtml &= renderVStyle(style, pathSelector("#"&rootId, id)).cstring

    proc removeEventListeners(vnode: VNode) =
      if not vnode.isText:
        for ekind, handler in vnode.handlers:
          vnode.node.removeEventListener(ekind.cstring, handler)

    proc update(currs, prevs: seq[VNode], parent: Node, id: NodeId)

    proc newNode(vnode: VNode, id: NodeId) =
      if vnode.isText:
        vnode.node = document.createTextNode(vnode.text.cstring)
      else:
        vnode.node = document.createElement(vnode.tag.cstring)
        vnode.updateStyles(@[], id)
        for attr, val in vnode.attributes:
          vnode.node.setAttr(attr.cstring, val.cstring)
        if "value" in vnode.attributes:
          vnode.node.value = vnode.attributes["value"].cstring
        for ekind, handler in vnode.handlers:
          vnode.node.addEventListener(ekind.cstring, handler)
        update(vnode.childs, @[], vnode.node, id)

    proc removeNode(vnode: VNode, parent: Node) =
      parent.removeChild(vnode.node)
      if vnode.stylesNode != nil:
        document.head.removeChild(vnode.stylesNode)


    proc update(curr, prev: VNode, parent: Node, id: NodeId, isRoot = false) =

      # keep root nodes id
      if isRoot:
        if "id" in curr.attributes and curr.attributes["id"] != rootId:
          raise ChangeRootElementIdError(msg: "trying to change id of root element")
        curr.attributes["id"] = rootId

      # completly replace node
      if (curr.isText xor prev.isText) or (not curr.isText and curr.tag != prev.tag):
        newNode(curr, id)
        parent.insertBefore(curr.node, prev.node)
        removeEventListeners(prev)
        removeNode(prev, parent)

      # just update node
      else:
        curr.node = prev.node

        if curr.isText:
          if curr.text != prev.text:
            curr.node.nodeValue = curr.text.cstring

        else:
          curr.stylesNode = prev.stylesNode
          curr.updateStyles(prev.styles, id)

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

          update(curr.childs, prev.childs, curr.node, id)

    proc update(currs, prevs: seq[VNode], parent: Node, id: NodeId) =
      var id = id & 0

      let commonLen = min(len(currs), len(prevs))

      # update nodes
      for i in 0 ..< commonLen:
        update(currs[i], prevs[i], parent, id)
        inc id[^1]

      # add new nodes
      if len(currs) > commonLen:
        for curr in currs[commonLen..^1]:
          newNode(curr, id)
          parent.appendChild(curr.node)
          inc id[^1]

      # remove extra nodes
      elif len(prevs) > commonLen:
        for prev in prevs[commonLen..^1]:
          removeEventListeners(prev)
          removeNode(prev, parent)

    let dom =
      if renderers[i].routing: renderers[i].buildProcRoute(route)
      else: renderers[i].buildProc()
    update(dom, renderers[i].prevDom, renderers[i].prevDom.node.parentNode, @[], true)
    renderers[i].prevDom = dom


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


  proc newRootNode(rootId: string): VNode =
    result = VNode(
      isText: false,
      node: document.getElementById(rootId)
    )
    result.tag = ($result.node.tagName).toLowerAscii

  proc setRenderer*(buildProc: proc: VNode, rootId = "ROOT") =
    renderers &= Renderer(
      routing: false,
      buildProc: buildProc,
      prevDom: newRootNode(rootId),
      rootId: rootId
    )
    redraw()

  proc setRenderer*(buildProc: proc(route: string): VNode, rootId = "ROOT") =
    renderers &= Renderer(
      routing: true,
      buildProcRoute: buildProc,
      prevDom: newRootNode(rootId),
      rootId: rootId
    )
    let i = high(renderers)
    window.addEventListener("hashchange") do (e: Event):
      redraw(i)

    redraw()