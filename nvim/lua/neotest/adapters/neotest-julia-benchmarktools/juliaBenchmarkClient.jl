using BenchmarkTools;
include("./benchmark/benchmarks.jl")
results = run(SUITE[ARGS[1]], verbose=true)
print(results)
