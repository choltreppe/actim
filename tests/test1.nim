import std/[dom, strformat, tables]
import actim


let style1 = newVStyle:
  "padding": fmt"{6.px} {8.px}"
  "padding-top": 5.px
  "background-color": "#44ffaa"

echo renderVStyle(style1, "#foo")

template repeateCount(n: int, body: untyped): VNode =
  buildVNode "div":
    for i in 1 .. n:
      ++ text(i, ":")
      body

proc buildDom: VNode =
  var testText {.global.} = "hi"

  buildVNode "div":
    style: newVStyle:
      "font-weight": "bold"

    handle "click":
      debugEcho vnode
      testText = "hey"

    ++ text testText

    ++ "a":
      attr "href": "/"

      ++ text "ho"
      ++ "br"
      ++ text "ha"

    +> repeateCount(3):
      ++ "b":
        ++ text "foo"
      ++ "br"

setRenderer buildDom
