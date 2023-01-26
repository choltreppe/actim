# Package

version       = "0.1.0"
author        = "Joel Lienhard"
description   = "A gui lib for web"
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 1.6.4"


task test, "test":
  exec "nim js tests/test1.nim"
  exec "nim js tests/test2.nim"

task gendoc, "generate docs":
  exec "nim doc --project --backend:js --docCmd:skip -o:docs src/actim.nim"
  withDir "docs":
    exec "mv actim.html index.html"
    exec "rm -r nimcache"