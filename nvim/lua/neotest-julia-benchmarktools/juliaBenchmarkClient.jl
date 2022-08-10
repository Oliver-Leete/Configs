using BenchmarkTools;
include(ARGS[1])
results = run(suite[ARGS[2]], verbose=true)
print(results)
