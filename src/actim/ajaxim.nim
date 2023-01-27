import std/dom
import ajax
import ./vdom


proc ajax*(meth, url: string, body = "", cb: proc(status: Natural, response: string)) =
  var xhr = newXmlhttpRequest()
  if xhr.isNil: return
  
  proc onRecv(e: Event) =
    if xhr.readyState == rsDONE:
      cb(xhr.status, $xhr.responseText)
      redraw()

  xhr.onReadyStateChange = onRecv
  xhr.open(meth, url)
  xhr.send(body.cstring)

proc ajaxGet*(url: string, cb: proc(status: Natural, response: string)) =
  ajax("GET", url, "", cb)

proc ajaxPost*(url, body: string, cb: proc(status: Natural, response: string)) =
  ajax("POST", url, body, cb)