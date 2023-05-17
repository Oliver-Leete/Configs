vim.cmd([[set errorformat=%E%tRROR:%m]])
-- vim.cmd([[set errorformat+=%Zin\ expression\ starting\ at\ %f:%l]])
vim.cmd([[set errorformat+=%-G%.%#@\ /.%#]])
vim.cmd([[set errorformat+=%Z%.%#@\ %f:%l]])
vim.cmd([[set errorformat+=%C%.%#]])
vim.cmd([[set errorformat+=%-G%.%#]])

vim.bo.commentstring = [[#%s]]

Jul_perf_flat = function() require("perfanno").load_traces(vim.json.decode(io.open("/tmp/jlprof.json", "rb"):read("*all"))) end

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

No_Using_Toggle = function(bp)
    local bp_pattern = "^[%s]*" .. bp .. ".*"

    if string.match(vim.fn.getline("."), bp_pattern) then
        vim.cmd("delete")
    else
        vim.cmd("normal! o" .. bp)
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

Map("n", ",dd", function() BP_Toggle("Debugger", "@bp") end, { buffer = 0 })
Map("n", ",di", function() No_Using_Toggle("Main.@infiltrate") end, { buffer = 0 })
Map("n", ",dq", function() BP_Remove_All({ "Debugger", "Infiltrator" }, { "@bp", "@infiltrate", "Main.@infiltrate" }) end,
    { buffer = 0 })

Map("n", "<leader>/", function()
    local path = vim.fn.expand("%")
    if vim.startswith(path, "docs/") then
        vim.cmd.edit({ args = { "docs/make.jl" } })
    elseif vim.startswith(path, "test/") then
        vim.cmd.edit({ args = { "test/runtests.jl" } })
    elseif vim.startswith(path, "benchmark/") then
        vim.cmd.edit({ args = { "benchmark/benchmarks.jl" } })
    else
        vim.cmd.edit({ args = { "src/" .. vim.g.project .. ".jl" } })
    end
end)
