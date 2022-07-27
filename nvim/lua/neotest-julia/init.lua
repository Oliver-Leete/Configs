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
           (macro_argument_list (string_literal) @test.name)
        ) @test.definition
    ]]

    return lib.treesitter.parse_positions(path, query, {
        require_namespaces = false,
        nested_tests = true,
    })
end

function adapter.build_spec(args)
    local position = args.tree:data()
    local command = "/home/oleete/.config/nvim/lua/neotest-julia/juliaTestRunner '" .. position.name .. "'"
    if position.type == "file" then
        return
    end
    return {
        command = command,
        context = {
            pos_id = position.id
        },
    }
end

function adapter.results(spec, result)
    local pos_id = spec.context.pos_id
    local data

    with(open("test-results.xml", "r"), function(reader)
        data = reader:read("*a")
    end)

    local handler = xml_tree:new()
    local parser = xml.parser(handler)
    parser:parse(data)

    local testcases
    if #handler.root.testsuites.testsuite.testcase == 0 then
        testcases = { handler.root.testsuites.testsuite.testcase }
    else
        testcases = handler.root.testsuites.testsuite.testcase
    end

    local results = {}
    for _, testcase in pairs(testcases) do
        if testcase.failure then
            results[testcase._attr.name] = {
                status = "failed",
                short = testcase.failure[1],
            }
        else
            results[testcase._attr.name] = {
                status = "passed",
            }
        end
    end

    return results
end

return adapter
