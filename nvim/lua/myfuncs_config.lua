
-- Repaets
-- set defaults
vim.api.nvim_set_var("dirJumps", "f")
vim.api.nvim_set_var("panelRepeat", "x")
vim.api.nvim_set_var("DiffviewLast", "DiffviewOpen")

function _G.commandRepeat(leader, varName)
    local jump = vim.api.nvim_get_var(varName)
    return vim.api.nvim_replace_termcodes(leader .. jump, true, true, true)
end

function _G.diff_repeat()
    local cmd = vim.api.nvim_get_var("DiffviewLast")
    vim.cmd(cmd)
end

-- Text object targets
function _G.ts_target(count, object)
    vim.cmd("TSTextobjectGotoNextStart " .. object)
    count = count-1
    while(count>0)
    do
        vim.cmd("TSTextobjectGotoNextStart " .. object)
        count = count-1
    end
    vim.cmd("TSTextobjectSelect " .. object)
end

function _G.ts_target_back(count, object)
    vim.cmd("TSTextobjectGotoPreviousEnd " .. object)
    count = count-1
    while(count>0)
    do
        vim.cmd("TSTextobjectGotoPreviousEnd " .. object)
        count = count-1
    end
    vim.cmd("TSTextobjectGotoPreviousStart " .. object)
    vim.cmd("TSTextobjectSelect " .. object)
end

-- Telescope

local action_state = require("telescope.actions.state")

local open_dif = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    local cmd = "DiffviewOpen " .. value
    vim.api.nvim_set_var("DiffviewLast", cmd)
    vim.cmd(cmd)
end
local open_dif_mergebase = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    local cmd = "DiffviewOpen ..." .. value
    vim.api.nvim_set_var("DiffviewLast", cmd)
    vim.cmd(cmd)
end
local open_single_dif = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    local cmd = "DiffviewOpen " .. value .. "~1.." .. value
    vim.api.nvim_set_var("DiffviewLast", cmd)
    vim.cmd(cmd)
end
local change_gitsign_base = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    vim.api.nvim_win_close(0, true)
    local cmd = "Gitsigns change_base " .. value
    vim.cmd(cmd)
end

function _G.gitsign_change_base()
    require("telescope.builtin").git_commits({
        attach_mappings = function(_, map)
            map("n", "<cr>", change_gitsign_base)
            map("i", "<cr>", change_gitsign_base)
            return true
        end,
    })
end
function _G.gitsign_bchange_base()
    require("telescope.builtin").git_bcommits({
        attach_mappings = function(_, map)
            map("n", "<cr>", change_gitsign_base)
            map("i", "<cr>", change_gitsign_base)
            return true
        end,
    })
end

function _G.git_commits_againsthead()
    require("telescope.builtin").git_commits({
        attach_mappings = function(_, map)
            map("n", "<cr>", open_dif)
            map("i", "<cr>", open_dif)
            return true
        end,
    })
end
function _G.git_commits_onechange()
    require("telescope.builtin").git_commits({
        attach_mappings = function(_, map)
            map("n", "<cr>", open_single_dif)
            map("i", "<cr>", open_single_dif)
            return true
        end,
    })
end

function _G.git_branch_dif()
    require("telescope.builtin").git_branches({
        attach_mappings = function(_, map)
            map("n", "<cr>", open_dif)
            map("i", "<cr>", open_dif)
            return true
        end,
    })
end
function _G.git_branch_mergebase()
    require("telescope.builtin").git_branches({
        attach_mappings = function(_, map)
            map("n", "<cr>", open_dif_mergebase)
            map("i", "<cr>", open_dif_mergebase)
            return true
        end,
    })
end

function _G.project_files()
    local opts = {} -- define here if you want to define something
    local ok = pcall(require("telescope.builtin").git_files, opts)
    if not ok then
        require("telescope.builtin").find_files(opts)
    end
end

