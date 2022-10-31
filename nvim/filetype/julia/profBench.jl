using BenchmarkTools, Profile, FileIO

cd(ARGS[1])

include(joinpath(ARGS[1], "benchmark/benchmarks.jl"))

eval(Meta.parse(ARGS[3]))
func = Meta.parse(ARGS[2])

eval(func)
@profile eval(func)

using JSON

function make_json(profileData)
    data = profileData[1]
    lidict = profileData[2]

    event = []
    for (key, stackframes) in lidict
        frames = []
        for stackframe in stackframes
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
        c = count(==(key), data)
        push!(event, Dict("count" => c, "frames" => frames))
    end
    ret = JSON.json(Dict("event 1" => event))
    return ret
end

write("/tmp/jlprof.json", make_json(Profile.retrieve()))
