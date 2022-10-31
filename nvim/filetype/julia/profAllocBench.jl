using BenchmarkTools, Profile, FileIO

cd(ARGS[1])

include(joinpath(ARGS[1], "benchmark/benchmarks.jl"))

eval(Meta.parse(ARGS[3]))
func = Meta.parse(ARGS[2])

eval(func)
@profile eval(func)

using JSON

function make_json(profileData)
    allocs = profileData.allocs
    counts = []
    size = []
    for data in profileData.allocs
        s = data.size
        c = count(x -> x.stacktrace==data.stacktrace, allocs)
        frames = []
        for stackframe in data.stacktrace
            if stackframe.line > 0
                push!(frames, Dict(
                    "symbol" => stackframe.func,
                    "file" => stackframe.file,
                    "linenr" => stackframe.line,
                ))
            else
                push!(frames, Dict(
                    "symbol" => stackframe.func,
                    "file" => stackframe.file,
                ))
            end
        end
        push!(counts, Dict("count" => c, "frames" => frames))
        push!(size, Dict("count" => s, "frames" => frames))
    end
    ret = JSON.json(Dict("size" => size, "count" => counts))
    return ret
end

write("/tmp/jlprof.json", make_json(Profile.Allocs.fetch()))
