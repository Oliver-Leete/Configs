using Profile

include(ARGS[1])
@profile include(ARGS[1])
Profile.print(open("/tmp/julprof.data", "w"), format=:flat)
