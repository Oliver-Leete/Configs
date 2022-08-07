local async = require("neotest.async")
local context_manager = require("plenary.context_manager")
local lib = require("neotest.lib")
local open = context_manager.open
local Path = require("plenary.path")
local with = context_manager.with
local xml = require("neotest.lib.xml")
local xml_tree = require("neotest.lib.xml.tree")

local adapter = { name = "neotest-julia" }

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
    local command = "/home/oleete/.config/nvim/lua/neotest-julia-benchmarktools/juliaBenchmarkRunner '" .. position.name .. "'"
    if position.type == "file" then
        return
    end
    return {
        command = command,
        error_file = error_file,
        context = {
            pos_id = position.id
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
