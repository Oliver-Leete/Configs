require("neogit").setup({
    log_view = {
        kind = "vsplit",
    },
    reflog_view = {
        kind = "vsplit",
    },

    signs = {
        hunk = { "", "" },
        item = { "", "" },
        section = { "", "" },
    },
    mappings = {
        status = {
            ["<space>"] = "Toggle",
        }
    }
})
require("gitsigns").setup({
    sign_priority = 6,
    _signs_staged_enable = true,
})

local gitstuff = vim.api.nvim_create_augroup("GitStuff", { clear = true })
vim.api.nvim_create_autocmd('User', {
  pattern = "NeogitCommitComplete",
  group = gitstuff,
  callback = require("gitsigns").refresh,
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
            ["<c-j>"] = actions.scroll_view(5),
            ["<c-k>"] = actions.scroll_view(-5),
            ["<esc>"] = function()
                vim.b[vim.api.nvim_get_current_buf()].is_diffview_file = false
                vim.cmd("DiffviewClose")
            end,
        },
        file_history_panel = {
            ["<c-j>"] = actions.scroll_view(5),
            ["<c-k>"] = actions.scroll_view(-5),
            ["<esc>"] = function() vim.cmd("DiffviewClose") end,
        },
    },
})

local Hydra = require("hydra")
local gitsigns = require("gitsigns")

local on = false
local toggle_show = function()
    if on then
        on = false
        gitsigns.toggle_linehl(false)
        gitsigns.toggle_deleted(false)
        gitsigns.toggle_word_diff(false)
    else
        on = true
        gitsigns.toggle_linehl(true)
        gitsigns.toggle_deleted(true)
        gitsigns.toggle_word_diff(true)
    end
end

Old_dir_jump = "search"
local hint = [[
┏^━━━━━━━━┳━━━━━┳━━━━━━━^┓
┃^        ┃ GIT ┃       ^┃
┃^        ┗━━━━━┛       ^┃
┃^         Hunks        ^┃
┣^━━━━━━━━━━━━━━━━━━━━━━^┫
┃^ n: next hunk         ^┃
┃^ N: prev hunk         ^┃
┃^                      ^┃
┃ _,s_: stage hunk       ┃
┃ _,r_: reset hunk       ┃
┃ _,u_: undo last stage  ┃
┃ _,S_: stage buffer     ┃
┃^                      ^┃
┃^          View        ^┃
┣^━━━━━━━━━━━━━━━━━━━━━━^┫
┃ _,K_: blame line       ┃
┃ _,p_: preview hunk     ┃
┃ _,d_: show diff        ┃
┣^━━━━━━━━━━━━━━━━━━━━━━^┫
┃ _,f_: file finder      ┃
┃ _,<esc>_: exit         ┃
┗^━━━━━━━━━━━━━━━━━━━━━━^┛
]]
Hydra({
    name = "Git",
    hint = hint,
    config = {
        color = "pink",
        invoke_on_body = true,
        hint = {
            position = "top-right",
            border = nil
        },
        on_enter = function()
            Old_dir_jump = vim.g.dirJumps
            vim.g.dirJumps = "h"
        end,
        on_exit = function()
            if vim.g.dirJumps == "h" then
                vim.g.dirJumps = Old_dir_jump
            end
            on = false
            gitsigns.toggle_linehl(false)
            gitsigns.toggle_deleted(false)
            gitsigns.toggle_word_diff(false)
        end,
    },
    mode = { "n", "x" },
    body = "<leader>g",
    heads = {
        { ",s",        gitsigns.stage_hunk,                          { silent = true, desc = "stage hunk" } },
        { ",r",        gitsigns.reset_hunk,                          { silent = true, desc = "stage hunk" } },
        { ",u",        gitsigns.undo_stage_hunk,                     { desc = "undo last stage" } },
        { ",S",        gitsigns.stage_buffer,                        { desc = "stage buffer" } },
        { ",p",        gitsigns.preview_hunk,                        { desc = "preview hunk" } },
        { ",d",        toggle_show,                                  { nowait = true, desc = "toggle diff" } },
        { ",K",        gitsigns.blame_line,                          { desc = "blame" } },
        { ",f",        "<cmd>Telescope git_status theme=get_ivy<cr>" },
        { ",<esc>",    nil,                                          { exit = true, nowait = true, desc = "exit" } },
        { "<leader>g", nil,                                          { exit = true, nowait = true, desc = false } },
    }
})
