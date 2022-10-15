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

using AbstractTrees
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
    for leaf in Leaves(flamegraph)
        push!(event, Dict(
            "count" => length(leaf.data.span),
            "frames" => [Dict(
                "symbols" => leaf.data.sf.func,
                "file" => leaf.data.sf.file,
                "linenr" => leaf.data.sf.line,
            ) for i in allparent(leaf)]
        ))
    end
    return event
end
