using Profile, FileIO
using ProfileCanvas

include(ARGS[1])
@profile include(ARGS[1])
Profile.print(
    IOContext(open("/tmp/julprof.data", "w"), :displaysize => (100000, 1000)),
    format=:flat
)
profileData = Profile.retrieve()
save("/tmp/last_jul.jlprof", profileData[1], profileData[2])
ProfileCanvas.view()

using AbstractTrees, FlameGraphs, JSON
function make_json(flamegraph)
    function allparent(node)
        ret = []
        while node.parent != node
            node = node.parent
            push!(ret, node)
        end
        return ret
    end
    event = []
    for leaf in PreOrderDFS(flamegraph)
        if leaf.data.sf.line != 0
            push!(event, Dict(
                "count" => length(leaf.data.span),
                "frames" => [Dict(
                    "symbol" => leaf.data.sf.func,
                    "file" => leaf.data.sf.file,
                    "linenr" => leaf.data.sf.line,
                ) for i in allparent(leaf)]
            ))
        else
            push!(event, Dict(
                "count" => length(leaf.data.span),
                "frames" => [Dict(
                    "symbol" => leaf.data.sf.func,
                    "file" => leaf.data.sf.file,
                ) for i in allparent(leaf)]
            ))
        end
    end
    ret = Dict( "event 1" => event )
    return ret
end

write("/tmp/jlprof.json", JSON.json(make_json(flamegraph(profileData[1], lidict=profileData[2]))))
