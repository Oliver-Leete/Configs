using BenchmarkTools;
include(joinpath(ARGS[1], "PackageBenchmarks.jl"))
results = run(suite[ARGS[2]], verbose=true)
print(results)
