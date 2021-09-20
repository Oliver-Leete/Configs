----------------------------------------------------------------------------------------------------
--                      _   _   ______    ____   __      __  _____   __  __                       --
--                     | \ | | |  ____|  / __ \  \ \    / / |_   _| |  \/  |                      --
--                     |  \| | | |__    | |  | |  \ \  / /    | |   | \  / |                      --
--                     | . ` | |  __|   | |  | |   \ \/ /     | |   | |\/| |                      --
--                     | |\  | | |____  | |__| |    \  /     _| |_  | |  | |                      --
--                     |_| \_| |______|  \____/      \/     |_____| |_|  |_|                      --
--                                                                                                --
----------------------------------------------------------------------------------------------------
-- Oliver Leete <oliverleete@gmail.com>                                                            --
-- https://github.com/oliver-leete                                                                 --
----------------------------------------------------------------------------------------------------

-- Nvim Tree
vim.g.nvim_tree_quit_on_open = 1
vim.g.nvim_tree_git_hl = 1
vim.g.nvim_tree_lsp_diagnostics = 1
vim.g.nvim_tree_width = 40
vim.g.nvim_tree_disable_window_picker = 1

vim.g.nvim_tree_icons = {
    default = "",
    symlink = "",
    git = { unstaged = "", staged = "✓", unmerged = "", renamed = "➜", untracked = "" },
    folder = { default = "", open = "", empty = "", empty_open = "", symlink = "" },
}
local tree_cb = require("nvim-tree.config").nvim_tree_callback
vim.g.nvim_tree_bindings = {
    { key = { "<CR>" }, cb = tree_cb("edit") },
    {
        key = { "o" },
        cb = "<cmd>lua require('nvim-tree').on_keypress('edit')<cr><cmd>sleep 250m<cr><cmd>NvimTreeClose<cr>",
    },
    { key = { "<2-LeftMouse>" }, cb = tree_cb("edit") },
    { key = { "<2-RightMouse>" }, cb = tree_cb("cd") },
    { key = { "<C-]>" }, cb = tree_cb("cd") },
    { key = { "<C-v>" }, cb = tree_cb("vsplit") },
    { key = { "<C-x>" }, cb = tree_cb("split") },
    { key = { "<C-t>" }, cb = tree_cb("tabnew") },
    { key = { "<" }, cb = tree_cb("prev_sibling") },
    { key = { ">" }, cb = tree_cb("next_sibling") },
    { key = { "<BS>" }, cb = tree_cb("close_node") },
    { key = { "<S-CR>" }, cb = tree_cb("close_node") },
    { key = { "<Tab>" }, cb = tree_cb("preview") },
    { key = { "I" }, cb = tree_cb("toggle_ignored") },
    { key = { "H" }, cb = tree_cb("toggle_dotfiles") },
    { key = { "R" }, cb = tree_cb("refresh") },
    { key = { "a" }, cb = tree_cb("create") },
    { key = { "dd" }, cb = tree_cb("remove") },
    { key = { "r" }, cb = tree_cb("rename") },
    { key = { "<C-r>" }, cb = tree_cb("full_rename") },
    { key = { "x" }, cb = tree_cb("cut") },
    { key = { "cc" }, cb = tree_cb("copy") },
    { key = { "p" }, cb = tree_cb("paste") },
    { key = { "[c" }, cb = tree_cb("prev_git_item") },
    { key = { "]c" }, cb = tree_cb("next_git_item") },
    { key = { "-" }, cb = tree_cb("dir_up") },
    { key = { "q" }, cb = tree_cb("close") },
    { key = { "<esc>" }, cb = tree_cb("close") },
}

-- Trouble Config

require("trouble").setup({
    height = 20,
    icons = true,
    mode = "workspace",
    fold_open = "",
    fold_closed = "",
    action_keys = {
        cancel = "q", -- cancel the preview and get back to your last window / buffer / cursor
        close = "<esc>", -- close the list
        refresh = "r", -- manually refresh
        jump = "<cr>", -- jump to the diagnostic or open / close folds
        jump_close = { "o" }, -- jump to the diagnostic and close the list
        toggle_mode = "m", -- toggle between "workspace" and "document" mode
        preview = "P", -- preview the diagnostic location
        toggle_preview = "p", -- preview the diagnostic location
        close_folds = { "zM", "zm" }, -- close all folds
        open_folds = { "zR", "zr" }, -- open all folds
        toggle_fold = { "zA", "za" }, -- toggle fold of current file
        previous = "k", -- preview item
        next = "j", -- next item
    },
    indent_lines = true, -- add an indent guide below the fold icons
    auto_open = false,
    auto_close = false,
    auto_preview = false, -- automatically preview the location of the diagnostic. <esc> to close preview and go back
    signs = {
        error = "",
        warning = "",
        information = "",
        hint = "",
    },
    use_lsp_diagnstic_signs = false, -- enabling this will use the s
})

-- Symbols Outline

vim.g.symbols_outline = {
    highlight_hovered_item = true,
    show_guides = true,
    position = "right",
    keymaps = {
        close = "<Esc>",
        goto_location = "<cr>",
        -- focus_location = "<cr>",
        hover_symbol = "<C-space>",
        rename_symbol = "r",
        code_actions = "a",
    },
    lsp_blacklist = {},
}
