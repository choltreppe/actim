# Package

version       = "0.1.0"
author        = "Joel Lienhard"
description   = "A gui lib for web"
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 1.6.4"
requires "ajax"


import std/strformat

task test, "test":
  for i in 1..2:
    exec fmt"nim js -d:nimPreviewHashRef tests/test{i}.nim"

task gendoc, "generate docs":
  exec "nim doc --project --backend:js --docCmd:skip -o:docs src/actim.nim"
  withDir "docs":
    exec "mv actim.html index.html"
    exec "rm -r nimcache"