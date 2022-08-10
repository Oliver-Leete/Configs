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
    local server_running = function()
        for _, task in pairs(task_list) do
            if task.name == "Julia Test Server" and task.status == "RUNNING" then
                return true
            end
        end
    end
    if not server_running() then
        require("overseer").run_template({ name = "Julia Test Server" })
    end


    local command = "cd test && julia --project -e'using DaemonMode; runargs()' "
        .. "/home/oleete/.config/nvim/lua/neotest-julia-retest/juliaTestClient.jl "
        .. vim.fn.getcwd() .. "/test/PackageTests.jl' "
        .. position.name:sub(2, -2)
        .. "'"

    if position.type == "file" then
        return
    end
    return {
        command = command,
        error_file = error_file,
        context = {
            pos_id = position.id,
            name = position.name,
            type = position.type,
        },
    }
end

function adapter.results(spec, result)
    Spec = spec
    local success, data = pcall(lib.files.read, result.output)
    if not success then
        return {}
    end

    data = data:match("Main.PackageTests\r\n(.*)$")
    local lines = vim.split(data, '\r\n')
    Lines = lines
    local ret_tab = {}
    for _, line in pairs(lines) do
        local pos_id, stat_icon = line:match("%d*|%s*(.*) (...)")
        if pos_id and stat_icon then
            local status
            if stat_icon == "✔" then
                status = 'passed'
            elseif stat_icon == "✘" then
                status = 'failed'
            end

            if spec.context.type == "test" then
                pos_id = spec.context.pos_id
            else
                pos_id = spec.context.pos_id .. "::\"" .. pos_id .. "\""
            end

            ret_tab[pos_id] = { status = status }
        end
    end
    RetTab = ret_tab

    return ret_tab
end

return adapter
