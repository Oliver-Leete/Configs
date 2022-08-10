local async = require("neotest.async")
local context_manager = require("plenary.context_manager")
local lib = require("neotest.lib")
local open = context_manager.open
local Path = require("plenary.path")
local with = context_manager.with
local xml = require("neotest.lib.xml")
local xml_tree = require("neotest.lib.xml.tree")

local adapter = { name = "neotest-julia-retest" }

adapter.root = lib.files.match_root_pattern("Project.toml")

function adapter.is_test_file(file_path)
    return vim.endswith(file_path, "ests.jl")
end

function adapter.discover_positions(path)
    local query = [[
        (macro_expression
           (macro_identifier (identifier) @macro_name)
           (#match? @macro_name "testcase")
           (macro_argument_list (string_literal) @test.name)
        ) @test.definition
        (macro_expression
           (macro_identifier (identifier) @macro_name)
           (#match? @macro_name "testset")
           (macro_argument_list (string_literal) @namespace.name)
        ) @namespace.definition
    ]]

    return lib.treesitter.parse_positions(path, query, {
        require_namespaces = false,
        nested_tests = true,
    })
end

function adapter.build_spec(args)
    local error_file = vim.fn.tempname()
    local position = args.tree:data()

    -- Make sure server is running
    local task_list = require("overseer.task_list").list_tasks()
    local server_running = false
    for _, task in pairs(task_list) do
        if task.name == "Julia Test Server" and task.status == "RUNNING" then
            server_running = true
        end
    end
    if not server_running then
        require("overseer").run_template({name = "Julia Test Server"})
    end

    local command = "/home/oleete/.config/nvim/lua/neotest-julia-retest/juliaTestRunner '" .. position.name:sub(2,-2) .. "'"
    if position.type ~= "test" then
        return
    end
    return {
        command = command,
        error_file = error_file,
        context = {
            pos_id = position.id,
            name = position.name,
        },
    }
end

function adapter.results(spec, result)
    local pos_id = spec.context.pos_id
    local status = result.code == 0 and "passed" or "failed"

    return { [pos_id] = {
        status = status,
    } }
end

return adapter
