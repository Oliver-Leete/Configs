using Profile

include(ARGS[1])
@profile include(ARGS[1])
Profile.print(open("/tmp/julprof.data", "w"), format=:flat)
run(`/home/oleete/.config/bin/nvrWS "lua _G.jul_perf_flat()"`);
