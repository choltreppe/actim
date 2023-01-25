runnableExamples:
  
  let style1 = addNewVStyle:
    padding 5.px
    backgroundColor {"#44ffaa"}

  template repeateCount(n: int, body: untyped): VNode =
    buildVNode tdiv:
      for i in 1 .. n:
        ++ text(i, ":")
        body

  proc buildDom: VNode =
    var testText {.global.} = "foo"

    buildVNode tdiv:
      style: addNewVStyle:
        fontWeight bold

      handle click:
        testText = "ba"

      ++ text testText

      ++ a:
        attr href: "/"
        ++ text "ho"

      ++ br

      +> repeateCount 3:
        style style1
        ++ text "foo"

  setRenderer buildDom


import actim/[vdom, vstyles]
export vdom, vstyles