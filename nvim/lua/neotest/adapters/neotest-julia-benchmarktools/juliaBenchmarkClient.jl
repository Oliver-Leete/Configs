using BenchmarkTools;
include(ARGS[1])
results = run(SUITE[ARGS[2]], verbose=true)
print(results)
