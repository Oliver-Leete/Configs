require("gitsigns").setup({
    signs = {
        add          = { text = "┃" },
        change       = { text = "┃" },
        delete       = { text = "╽" },
        topdelete    = { text = "╿" },
        changedelete = { text = "┃" },
        untracked    = { text = "┋" },
    },
    _signs_staged = {
        add          = { text = '│' },
        change       = { text = '│' },
        delete       = { text = '│' },
        topdelete    = { text = '│' },
        changedelete = { text = '│' },
    },
    sign_priority = 6,
    _signs_staged_enable = true,
    preview_config = {
        border = Border,
    }
})

-- DiffView.nvim
local actions = require("diffview.config").actions

require("diffview").setup({
    diff_binaries = false,
    enhanced_diff_hl = true,
    use_icons = true,
    hooks = {
        diff_buf_read = function(bufnr)
            vim.b[bufnr].is_diffview_file = true
        end,
        view_opened = function(view)
            if view.panel and view.panel.bufname == "DiffviewFileHistoryPanel" then
                vim.cmd.LualineRenameTab({args = {"Git History"}})
            elseif view.panel and view.panel.bufname == "DiffviewFilePanel" then
                vim.cmd.LualineRenameTab({args = {"Git Changes"}})
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
