runnableExamples:
  let style1 = addNewVStyle:
    padding 5.px
    backgroundColor {"#44ffaa"}

  proc buildDom =
    var testText {.global.} = "foo"

    vn tdiv:
      style: addNewVStyle:
        fontWeight bold

      handle click:
        testText = "ba"

      text testText

      vn a:
        attr href: "/"
        text "ho"

    vn tdiv:
      style style1

      text "foo"
      vn br
      text "ba"

  setRenderer buildDom


import actim/[vdom, vstyles]
export vdom, vstyles