using Profile

include(ARGS[1])
# func = getfield(Main, Symbol(ARGS[2]))
# args = ARGS[3:end]
# func(args...)
# @profile func(args...)
@profile include(ARGS[1])
Profile.print(open("/tmp/julprof.data", "w"), format=:flat)
