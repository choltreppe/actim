import std/[dom]
import actim


let style1 = addNewVStyle:
  padding 5.px
  backgroundColor {"#44ffaa"}

proc buildDom =
  var testText {.global.} = "hi"

  vn tdiv:
    style: addNewVStyle:
      fontWeight bold
    handle click:
      debugEcho vnode
      testText = "hey"
    text testText
    vn a:
      attr href: "/"
      text "ho"
      vn br
      text "ha"
  vn tdiv:
    style style1
    text "bla"

setRenderer buildDom
