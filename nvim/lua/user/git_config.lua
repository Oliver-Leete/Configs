require("gitsigns").setup({
    sign_priority = 6,
    signs = {
        add = { hl = 'GitSignsAdd', text = '▌', numhl = 'GitSignsAddNr', linehl = 'GitSignsAddLn' },
        change = { hl = 'GitSignsChange', text = '▌', numhl = 'GitSignsChangeNr', linehl = 'GitSignsChangeLn' },
        delete = { hl = 'GitSignsDelete', text = '▁', numhl = 'GitSignsDeleteNr', linehl = 'GitSignsDeleteLn' },
        topdelete = { hl = 'GitSignsDelete', text = '▔', numhl = 'GitSignsDeleteNr', linehl = 'GitSignsDeleteLn' },
        changedelete = { hl = 'GitSignsChange', text = '~', numhl = 'GitSignsChangeNr', linehl = 'GitSignsChangeLn' },
    },
    preview_config = {
        border = Border,
        title = " Git ",
        title_pos = "center",
    },
})

-- DiffView.nvim
local actions = require("diffview.config").actions
require("diffview").setup({
    diff_binaries = false,
    use_icons = true,
    hooks = {
        diff_buf_read = function(bufnr)
            vim.b[bufnr].is_diffview_file = true
        end,
        view_opened = function(view)
            if view.panel and view.panel.bufname == "DiffviewFileHistoryPanel" then
                vim.t[vim.api.nvim_get_current_tabpage()].tabname = "Git History"
            elseif view.panel and view.panel.bufname == "DiffviewFilePanel" then
                vim.t[vim.api.nvim_get_current_tabpage()].tabname = "Git Changes"
            end
        end
    },
    key_bindings = {
        view = {
            ["<esc>"] = actions.focus_files,
            [",xo"] = actions.conflict_choose("ours"),
            [",xt"] = actions.conflict_choose("theirs"),
            [",xb"] = actions.conflict_choose("base"),
            [",xa"] = actions.conflict_choose("all"),
            [",xn"] = actions.conflict_choose("none"),
        },
        file_panel = {
            ["<esc>"] = function() vim.cmd("DiffviewClose") end,
        },
        file_history_panel = {
            ["<esc>"] = function() vim.cmd("DiffviewClose") end,
        },
    },
})

local Hydra = require("hydra")
local gitsigns = require('gitsigns')

Old_dir_jump = "search"
local hint = [[
 n: next hunk
 N: prev hunk

 _s_: stage hunk
 _R_: reset hunk
 _u_: undo last stage

 _S_: stage buffer
^^━━━━━━━━━━━━━━━━━━━━
 _b_: blame line
 _B_: blame show full
 _p_: preview hunk

 _d_: show deleted
 _w_: show word diff
^^━━━━━━━━━━━━━━━━━━━━
 _f_: file finder
 _<esc>_: exit
]]
Hydra({
    name = 'Git',
    hint = hint,
    config = {
        color = 'pink',
        invoke_on_body = true,
        hint = {
            position = "middle-right",
            border = Border
        },
        on_enter = function()
            Old_dir_jump = vim.g.dirJumps
            vim.g.dirJumps = "h"
            gitsigns.toggle_linehl(true)
        end,
        on_exit = function()
            vim.g.dirJumps = Old_dir_jump
            gitsigns.toggle_linehl(false)
            gitsigns.toggle_deleted(false)
            gitsigns.toggle_word_diff(false)
        end,
    },
    mode = { 'n', 'x' },
    body = '<leader>g',
    heads = {
        { 's', gitsigns.stage_hunk, { silent = true, desc = 'stage hunk' } },
        { 'R', gitsigns.reset_hunk, { silent = true, desc = 'stage hunk' } },
        { 'u', gitsigns.undo_stage_hunk, { desc = 'undo last stage' } },
        { 'S', gitsigns.stage_buffer, { desc = 'stage buffer' } },
        { 'p', gitsigns.preview_hunk, { desc = 'preview hunk' } },
        { 'd', gitsigns.toggle_deleted, { nowait = true, desc = 'toggle deleted' } },
        { 'b', gitsigns.blame_line, { desc = 'blame' } },
        { 'B', function() gitsigns.blame_line({ full = true }) end, { desc = 'blame show full' } },
        { '<esc>', nil, { exit = true, nowait = true, desc = 'exit' } },
        { "w", gitsigns.toggle_word_diff },
        { "f", "<cmd>Telescope git_status theme=get_ivy<cr>" },
        { '<leader>g', nil, { exit = true, nowait = true, desc = false } },
    }
})
