local lib = require("neotest.lib")
local async = require("neotest.async")

local adapter = { name = "neotest-julia-testitem" }

adapter.root = lib.files.match_root_pattern("Project.toml")

function adapter.is_test_file(file_path)
    return vim.endswith(file_path, ".jl")
end

function adapter.discover_positions(path)
    local query = [[
        (macrocall_expression
           (macro_identifier (identifier) @macro_name)
           (#match? @macro_name "testitem")
           (macro_argument_list (string_literal) @test.name
           (compound_statement (_)))
        ) @test.definition
    ]]

    return lib.treesitter.parse_positions(path, query, {
        require_namespaces = false,
        nested_tests = true,
    })
end

function adapter.build_spec(args)
    local position = args.tree:data()
    local res_file = async.fn.tempname() .. ".json"

    local command = "julia --color=yes --project -e '" ..
        [[using DaemonMode;
        try; runexpr("Revise.revise(throw=true)"); catch; sendExitCode(); end;
        runargs()]]
        .. "' /home/oleete/.config/nvim/lua/neotest/adapters/neotest-julia-testitem/juliaTestClient.jl "
        .. position.name
        .. " '" .. res_file .. "'"

    if position.type == "file" then return end
    return {
        command = command,
        context = {
            pos_id = position.id,
            name = position.name,
            uses_server = true,
            tsk_name = position.name:sub(2, -2) .. " test",
            res_file = res_file
        },
    }
end

function adapter.results(spec, result)
    local pos_id = spec.context.pos_id
    local res_file = io.open(spec.context.res_file, "rb")
    if not res_file then return { [pos_id] = { status = "skipped" } } end
    local data = vim.json.decode(res_file:read("*all"))
    if not data or not data.passed then return { [pos_id] = { status = "failed" } } end

    return { [pos_id] = {
        status = data.passed and "passed" or "failed",
    } }
end

return adapter
