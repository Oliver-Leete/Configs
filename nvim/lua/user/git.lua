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
