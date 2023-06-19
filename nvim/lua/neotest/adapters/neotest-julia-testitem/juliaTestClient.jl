using TestItemRunner
using JSON

function make_dict(a)
    res_list = []
    if length(a.results) > 0
        for r in a.results
            push!(res_list, make_dict(r))
        end
    end
    return Dict(
        "name" => a.description,
        "passed" => !a.anynonpass,
        "results" => res_list,
    )
end

try
    result = @run_package_tests verbose=true filter = ti -> (ti.name == ARGS[1]);
    write(ARGS[2], JSON.json(make_dict(result)))
catch
    write(ARGS[2], JSON.json(Dict("passed" => false)))
end
