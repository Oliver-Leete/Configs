using BenchmarkTools, Profile, FileIO

cd(ARGS[1])

include(joinpath(ARGS[1], "benchmark/benchmarks.jl"))

eval(Meta.parse(ARGS[3]))
func = Meta.parse(ARGS[2])

eval(func)
a = @profile eval(func)

using FlameGraphs, AbstractTrees, JSON

function make_json(profileData)
    flame = flamegraph(profileData[1], lidict=profileData[2])
    function allparent(node)
        ret = []
        while node.parent != node
            node = node.parent
            push!(ret, node)
        end
        return ret
    end
    event = []
    for node in PreOrderDFS(flame)
        # calclate span of node minus span of all children
        count = length(node.data.span)
        for child in children(node)
            count -= length(child.data.span)
        end
        if count > 0
            push!(event, Dict(
                "count" => count,
                "frames" => [i.data.sf.line > 0 ?
                             Dict("symbol" => i.data.sf.func, "file" => i.data.sf.file, "linenr" => i.data.sf.line,) :
                             Dict("symbol" => i.data.sf.func, "file" => i.data.sf.file,)
                             for i in allparent(node)]))
        end
    end
    ret = JSON.json(Dict("event 1" => event))
    return ret
end

write("/tmp/jlprof.json", make_json(Profile.retrieve()))