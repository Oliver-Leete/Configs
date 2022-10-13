vim.cmd([[set errorformat=%E%tRROR:%m]])
-- vim.cmd([[set errorformat+=%Zin\ expression\ starting\ at\ %f:%l]])
vim.cmd([[set errorformat+=%-G%.%#@\ /.%#]])
vim.cmd([[set errorformat+=%Z%.%#@\ %f:%l]])
vim.cmd([[set errorformat+=%C%.%#]])
vim.cmd([[set errorformat+=%-G%.%#]])

vim.bo.commentstring = [[#%s]]


function Jul_perf_flat()
    local function get_command_output(cmd, silent)
        if silent then
            cmd = cmd .. " 2>/dev/null"
        end

        local data_file = assert(io.popen(cmd, "r"))
        data_file:flush()
        local output = data_file:read("*all")
        data_file:close()

        return output
    end

    local perf_data = "/tmp/julprof.data"
    local raw_data = get_command_output("cat " .. perf_data, true)

    local result = {}
    local current_event = 1
    result[1] = {}

    for line in raw_data:gmatch("[^\r\n]+") do
        local count, file, linenr, symbol = line:match("^%s*(%d+)%s+%d+%s+(.-)%s+(%d+)%s+(.*)")
        local success = count and file and linenr and symbol

        local cur_file_name = vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf())

        if success and tonumber(count) > 0 then
            if file:find("@" .. vim.g.project) then
                file = "/home/oleete/Projects/" .. file.match(file, "@(" .. vim.g.project .. ".*)")
                local trace = { symbol = symbol, file = file, linenr = tonumber(linenr) }

                table.insert(result[current_event], { count = tonumber(count), frames = { trace } })
            elseif file:find(cur_file_name) then
                file = vim.api.nvim_buf_get_name(vim.api.nvim_get_current_buf())
                local trace = { symbol = symbol, file = file, linenr = tonumber(linenr) }

                table.insert(result[current_event], { count = tonumber(count), frames = { trace } })
            end
        end
    end
    require("perfanno").load_traces(result)
end

Run_closest = function()
    -- TODO: make use treesitter
    -- TODO: run nearest test if nearer than function
    -- TODO: make the logic of test location better
    local func_pat = "function [^%s]"
    local func_num = vim.fn.search(func_pat, "nbWc")

    local ass_pat = "[^%s](.*) = "
    local ass_num = vim.fn.search(ass_pat, "nbWc")

    local struct_pat = "struct [^%s]"
    local struct_num = vim.fn.search(struct_pat, "nbWc")

    local line_num = math.max(func_num, ass_num, struct_num)

    if line_num == 0 then
        return
    end

    local line = vim.fn.getline(line_num)
    local name
    if struct_num < math.max(func_num, ass_num) then
        name = line:match("([^%s]+)%(")
    else
        name = line:match("struct ([%w^_-]+)")
    end
    local server_running = function()
        local task_list = require("overseer.task_list").list_tasks()
        for _, task in pairs(task_list) do
            if task.metadata.is_test_server and task.status == "RUNNING" then
                return true
            end
        end
    end
    if not server_running() then
        require("overseer").run_template({ name = "Julia test server" })
    end
    local location = vim.fn.expand("%:p:r") .. "_tests.jl::\"" .. name .. '"'
    location = location:gsub("/src/", "/test/")
    require("neotest").run.run(location)
end

local bp_remover = function(imp_line, bp_pattern, mod_pat)
    vim.cmd("delete")

    -- if there are no bps left, remove the import
    local mod_1 = vim.fn.search(mod_pat, "nbW")
    local mod_2 = vim.fn.search(mod_pat, "ncW")

    local bp_1 = vim.fn.search(bp_pattern, "nbW")
    local bp_2 = vim.fn.search(bp_pattern, "ncW")

    if (bp_1 == 0 or (bp_1 < mod_1 and mod_1 ~= 0)) and (bp_2 == 0 or (bp_2 > mod_2 and mod_2 ~= 0)) then
        local imp_num = vim.fn.search(imp_line, "nbW")

        if imp_num < mod_1 then
            return
        end

        local cur_pos = vim.fn.getcurpos()
        vim.cmd(imp_num .. "delete")
        vim.fn.cursor(cur_pos[2] - 2, cur_pos[3])
    end
end

local bp_adder = function(bp, imp_line, mod_pat)
    local mod_num = vim.fn.search(mod_pat, "nbW")
    local imp_num = vim.fn.search(imp_line, "nbW")
    if imp_num == 0 or imp_num < mod_num then
        vim.fn.append(mod_num, imp_line)
    end

    vim.cmd("normal! o" .. bp)
end

BP_Toggle = function(imp_name, bp)
    local mod_pat = "^[%s]*module .*"

    local imp_line = "using " .. imp_name
    local bp_pattern = "^[%s]*" .. bp .. ".*"

    if string.match(vim.fn.getline("."), bp_pattern) then
        bp_remover(imp_line, bp_pattern, mod_pat)
    else
        bp_adder(bp, imp_line, mod_pat)
    end
end

BP_Remove_All = function(imp_names, bp_names)
    local cur_pos = vim.fn.getcurpos()
    for _, imp_name in pairs(imp_names) do
        local imp_line = "using " .. imp_name
        local imp_num = vim.fn.search(imp_line, "nw")

        while imp_num ~= 0 do
            vim.cmd(imp_num .. "delete")
            imp_num = vim.fn.search(imp_line, "nw")
        end
    end

    for _, bp_name in pairs(bp_names) do
        local bp_line = "[%s]*" .. bp_name .. ".*"
        local bp_num = vim.fn.search(bp_line, "nw")

        while bp_num ~= 0 do
            vim.cmd(bp_num .. "delete")
            bp_num = vim.fn.search(bp_line, "nw")
        end
    end
    vim.fn.cursor(cur_pos[2] - 2, cur_pos[3])
end

Map("n", ",rb", "<cmd>call julia#toggle_function_blockassign()<cr>")

Map("n", "<leader>l", Run_closest, {  buffer = 0 })

Map("n", ",dd", function() BP_Toggle("Debugger", "@bp") end, { buffer = 0 })
Map("n", ",di", function() BP_Toggle("Infiltrator", "@infiltrate") end, { buffer = 0 })
Map("n", ",dq", function() BP_Remove_All({ "Debugger", "Infiltrator" }, { "@bp", "@infiltrate" }) end, { buffer = 0 })

vim.b.minisurround_config = {
    custom_surroundings = {
        ['a'] = {
            input = { "%(.-->.-%)", "^.-->%s?().-()[),\n]$" },
            output = { left = '(x -> ', right = ')' },
        },
    }
}
