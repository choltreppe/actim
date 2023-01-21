# Actim

Actim is a small, simple web frontend framework.<br>

## Example
```nim
import actim

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
```

It works by simply defining e renderer proc that generates a VDom. This renderer will be called whenever some event occurs. And the DOM will be updated where its needed.<br>
The way the VDom is constructed is inspired by [fidget](https://github.com/treeform/fidget).<br>
The VDom is build by adding VNodes to a global stack. Every node that gets added via the `vn` macro opens a new 'scope' on that stack, which gets collected into the childs of that node.

## Components

Defining components is really simple. Just write a proc/template/macro that generates the VDom.

### Examples

```nim
let textInputStyle = addNewVStyle:
  backgroundColor {"#444"}
  color white
  padding {20.px}

proc drawTextInput*(styles: varargs[VStyleId]) =
  vn input:
    attr type: "text"
    for s in styles:
      style s
    style textInputStyle
```

```nim
template drawNav*(body: untyped) =
  const
    navStyle = addNewVStyle:
      backgroundColor {"#bbb"}
      padding {10.px}

    optionStyleBase = newVStyle:
      padding {5.px}

    optionStyle = addExtendVStyle optionStyleBase:
      color {"#333"}

    selectedOptionStyle = addExtendVStyle optionStyleBase:
      color white
      backgroundColor black

  var 
    select {.global.} = 0
    optionNum = 0

  template option(title: static string, onclick: untyped) {.inject.} =
    vn tdiv:
      text title
      if optionNum == select:
        style selectedOptionStyle
      else:
        style optionStyle
        let n = optionNum
        handle click:
          select = n
          onclick

    inc optionNum

  vn tdiv:
    style navStyle
    body

# use:

proc buildDom =
  drawNav:
    attr id: "some-nav"

    option "foo":
      echo "clicked"

    option "ba": discard

setRenderer buildDom
```

### Usefull for more complex components
Some things that could be usefull for more complex componenets:<br>
[newVNode](https://choltreppe.github.io/actim/actim/vdom.html#newVNode%2Cstring)
[newVnodeWith](https://choltreppe.github.io/actim/actim/vdom.html#newVNodeWith.t%2CVNodeKind%2Cuntyped)
[addVNode](https://choltreppe.github.io/actim/actim/vdom.html#addVNode%2CVNode)

## Contributions
Issues and PRs are welcome.

## TODO
- Better documentation (espacially readme)
- Add routing (with hash part)
- Add an ajax module
