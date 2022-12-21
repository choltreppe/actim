import std/[dom]
import actim


let style1 = newVStyle:
  padding 5.px
  backgroundColor "#44ffaa"

proc buildDom =
  var testText {.global.} = "hi"

  vn tdiv:
    style: newVStyle:
      fontWeight "bold"
    handle click:
      debugEcho node
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
