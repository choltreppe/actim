import std/[dom]
import actim

proc buildDom(route: string): VNode =

  buildVNode "div":
    ++ text("route: ", route)
    ++ "br"
    ++ "a":
      attr "href": "#bla"
      ++ text "click me"

setRenderer buildDom
