import std/[dom]
import actim


let style1 = newStyle:
  padding 5.px
  backgroundColor "#44ffaa"

proc buildDom =
  var testText {.global.} = "hi"

  tdiv:
    style.add: newStyle:
      fontWeight "bold"
    handlers[onclick] = proc(e: Event, n: VNode) =
      debugEcho "yo"
      testText = "hey"
    text testText
    a:
      text "ho"
      br()
      text "ha"
  tdiv:
    style &= style1
    text "bla"

setRenderer buildDom
