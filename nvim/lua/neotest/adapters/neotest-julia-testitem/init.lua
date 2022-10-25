local lib = require("neotest.lib")

local adapter = { name = "neotest-julia-testitem" }

adapter.root = lib.files.match_root_pattern("Project.toml")

function adapter.is_test_file(file_path)
    return vim.endswith(file_path, ".jl")
end

function adapter.discover_positions(path)
    local query = [[
        (macro_expression
           (macro_identifier (identifier) @macro_name)
           (#match? @macro_name "testitem")
           (macro_argument_list (string_literal) @test.name
           (compound_expression (_)) @test.definition)
        )
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
    local server_running = function()
        local task_list = require("overseer.task_list").list_tasks()
        for _, task in pairs(task_list) do
            if task.metadata.is_test_server and task.status == "RUNNING" then
                return true
            end
        end
    end
    if not server_running() then
        require("overseer").run_template({ name = "Start Test Server" })
    end


    local command = "julia --color=yes --project -e '" ..
        [[using DaemonMode;
        try; runexpr("Revise.revise(throw=true)"); catch; sendExitCode(); end;
        runargs()]]
        .. "' /home/oleete/.config/nvim/lua/neotest/adapters/neotest-julia-testitem/juliaTestClient.jl "
        .. position.name

    if position.type == "file" then
        return
    end
    return {
        command = command,
        error_file = error_file,
        context = {
            pos_id = position.id,
            name = position.name,
            uses_server = true,
            tsk_name = position.name:sub(2, -2) .. " test",
        },
    }
end

function adapter.results(spec, result)
    local pos_id = spec.context.pos_id
    return { [pos_id] = {
        status = result.code == 0 and "passed" or "failed",
    } }
end

return adapter
