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
