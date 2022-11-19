local lib = require("neotest.lib")

local adapter = { name = "neotest-julia-benchmarktools" }

adapter.root = lib.files.match_root_pattern("Project.toml")

function adapter.is_test_file(file_path)
    return vim.endswith(file_path, "enchmarks.jl")
end

function adapter.discover_positions(path)
    local query = [[
    (assignment_expression
        (index_expression
           (string_literal) @test.name)
        (_)
    )@test.definition
    ]]

    return lib.treesitter.parse_positions(path, query, {
        require_namespaces = false,
        nested_tests = true,
    })
end

function adapter.build_spec(args)
    local position = args.tree:data()

    local command = "julia --project -e '" ..
        [[using DaemonMode, Revise
        try
            runexpr("Revise.revise(throw=true)")
        catch
            sendExitCode()
        end
        runargs()' ]]
        .. "/home/oleete/.config/nvim/lua/neotest/adapters/neotest-julia-benchmarktools/juliaBenchmarkClient.jl "
        .. position.name

    if position.type == "file" then
        return
    end
    return {
        command = command,
        context = {
            pos_id = position.id,
            name = position.name,
            uses_server = true,
            tsk_name = position.name:sub(2,-2) .. " benchmark",
        },
    }
end

function adapter.results(spec, result)
    local success, data = pcall(lib.files.read, result.output)
    if not success then return {} end

    local time = data:match("%((.*)%)")
    local pos_id = spec.context.pos_id
    local status = result.code == 0 and "passed" or "failed"

    return { [pos_id] = {
        status = status,
        time = time,
    } }
end

return adapter
