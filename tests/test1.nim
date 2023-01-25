import std/[dom]
import actim


let style1 = addNewVStyle:
  padding 5.px
  backgroundColor {"#44ffaa"}

template repeateCount(n: int, body: untyped): VNode =
  buildVNode tdiv:
    for i in 1 .. n:
      ++ text(i, ":")
      body

proc buildDom: VNode =
  var testText {.global.} = "hi"

  buildVNode tdiv:
    style: addNewVStyle:
      fontWeight bold
    handle click:
      debugEcho vnode
      testText = "hey"

    ++ text testText

    ++ a:
      attr href: "/"

      ++ text "ho"
      ++ br
      ++ text "ha"

    +> repeateCount(3):
      ++ bold:
        ++ text "foo"
      ++ br

setRenderer buildDom
