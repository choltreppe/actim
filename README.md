**This is in early development, so use with caution**

# Actim

Actim is a small, simple web frontend framework.

Actim works by simply defining a renderer proc that generates a VNode. This renderer will be called whenever some event occurs. And the DOM will be updated where its needed (dom diffing).

## Example
```nim
import actim

let style1 = newVStyle:
  "padding": 5.px
  "background-color": "#44ffaa"

template repeateCount(n: int, body: untyped): VNode =
  buildVNode "div":
    for i in 1 .. n:
      ++ text(i, ":")
      body

proc buildDom: VNode =
  var testText {.global.} = "foo"

  buildVNode "div":
    style: newVStyle:
      "font-weight": "bold"

    handle "click":
      testText = "ba"

    ++ text testText

    ++ "a":
      attr "href": "/"
      ++ text "ho"

    ++ "br"

    +> repeateCount 3:
      style style1
      ++ text "foo"

setRenderer buildDom
```

## Implicit vnode var

In every scope you have access to an implicit `vnode` variable, which is the currently constructed vnode.

So lets look at a short example by rewriting the following code:
```nim
buildVNode "div":
  style: newVStyle:
    "font-weight": "bold"

  handle "click":
    echo "click"

  ++ text "foo"
```
to
```nim
buildVNode "div":
  vnode.styles.add: newVStyle:
    "font-weight": "bold"

  vnode.handlers["click"] = proc(e: Event) =
    echo "click"

  vnode.childs &= text "foo"
```

In practice this is primarly usefull inside handlers, but maybe there are some other special cases where this comes in handy. If you need that manual access its there.

## Components

Defining components is really simple. Just write a proc/template/macro that generates the VDom.

### Examples

```nim
let textInputStyle = newVStyle:
  "background-color": "#444"
  "color": "white"
  "padding": 20.px

proc drawTextInput*(styles: varargs[VStyle]): VNode =
  buildVNode "input":
    attr "type": "text"
    for s in styles:
      style s
    style textInputStyle
```

```nim
template drawNav*(body: untyped): VNode =
  let
    navStyle = newVStyle:
      "background-color": "#bbb"
      "padding": 10.px

    optionStyleBase = newVStyle:
      "padding": 5.px

    optionStyle = extendVStyle optionStyleBase:
      "color": "#333"

    selectedOptionStyle = extendVStyle optionStyleBase:
      "color": "white"
      "background-color": "black"

  var 
    select {.global.} = 0
    optionNum = 0

  template option(title: static string, onclick: untyped) {.inject.} =
    ++ "div":
      ++ text title
      if optionNum == select:
        style selectedOptionStyle
      else:
        style optionStyle
        let n = optionNum
        handle "click":
          select = n
          onclick

    inc optionNum

  buildVNode "div":
    style navStyle
    body

# use:

proc buildDom: VNode =
  drawNav:

    option "foo":
      echo "clicked"

    option "ba": discard

setRenderer buildDom
```

## Routing

To use routing via the hash part of the url, use a `proc(route: string): VNode` as renderer.<br>
Take a look at `tests/test2.nim` for a small example.

## Contributions
Issues and PRs are welcome.

## TODO
- Better documentation (espacially readme)
- complete ajax module
