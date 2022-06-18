using Pkg

Pkg.activate("docs")

using LiveServer, Base.ARGS[1]


Sys.isapple() ? run(`open $(Base.ARGS[2])`) : Sys.iswindows() ? run(`cmd /c start $(Base.ARGS[2])`) : Sys.islinux() ? run(`xdg-open $(Base.ARGS[2])`) : nothing
