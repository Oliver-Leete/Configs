using BenchmarkTools, Profile, FileIO
using ProfileView

a = @bprofile include(ARGS[1])

using JSON

function make_json(profileData)
    data = profileData[1]
    lidict = profileData[2]

    event = []
    for (key, stackframes) in lidict
        c = count(==(key), data)
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
        push!(event, Dict("count" => c, "frames" => frames))
    end
    ret = JSON.json(Dict("event 1" => event))
    return ret
end

write("/tmp/jlprof.json", make_json(Profile.fetch()))
ProfileView.view()
display(a)
