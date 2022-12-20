# Package

version       = "0.1.0"
author        = "Joel Lienhard"
description   = "A gui lib for web"
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 1.6.4"


task test, "":
  exec "nim js tests/test1.nim"