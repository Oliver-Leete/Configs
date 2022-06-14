-- HACK: #104 Invalid in command-line window
local hl = require("todo-comments.highlight")
local highlight_win = hl.highlight_win
hl.highlight_win = function(win, force)
	pcall(highlight_win, win, force)
end
require("todo-comments").setup({
    signs = true,
    keywords = {
        FIX = { icon = " ", color = "error", alt = { "FIXME", "BUG", "FIXIT", "FIX", "ISSUE" } },
        TODO = { icon = " ", color = "info" },
        HACK = { icon = " ", color = "warning", alt = { "JANK", "WORKAROUND" } },
        WARN = { icon = " ", color = "warning", alt = { "WARNING" } },
        PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
        NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
    },
    highlight = {
        before = "",
        keyword = "bg",
        after = "bg",
    },
    colors = {
        error = { "LspDiagnosticsDefaultError", "ErrorMsg", "#DC2626" },
        warning = { "LspDiagnosticsDefaultWarning", "WarningMsg", "#FBBF24" },
        info = { "LspDiagnosticsDefaultInformation", "#2563EB" },
        hint = { "LspDiagnosticsDefaultHint", "#10B981" },
        default = { "Identifier", "#7C3AED" },
    },
})
local lib = require("nvim-tree.lib")

local git_add = function()
    local node = lib.get_node_at_cursor()
    local gs = node.git_status

    -- If the file is untracked, unstaged or partially staged, we stage it
    if gs == "??" or gs == "MM" or gs == "AM" or gs == " M" then
        vim.cmd("silent !git add " .. node.absolute_path)

        -- If the file is staged, we unstage
    elseif gs == "M " or gs == "A " then
        vim.cmd("silent !git restore --staged " .. node.absolute_path)
    end

    lib.refresh_tree()
end

require("nvim-tree").setup({
    view = {
        mappings = {
            custom_only = true,
            list = {
                { key = { "<CR>", "<2-LeftMouse>" }, action = "edit_no_picker" },
                { key = { "<C-CR>" }, action = "cd" },
                { key = "<C-v>", action = "vsplit" },
                { key = "<C-x>", action = "split" },
                { key = "<C-t>", action = "tabnew" },
                { key = "<", action = "parent_node" },
                { key = "<BS>", action = "close_node" },
                { key = "<Tab>", action = "preview" },
                { key = "I", action = "toggle_git_ignored" },
                { key = "H", action = "toggle_dotfiles" },
                { key = "R", action = "refresh" },
                { key = "o", action = "create" },
                { key = "d", action = "remove" },
                { key = "r", action = "rename" },
                { key = "c", action = "copy" },
                { key = "p", action = "paste" },
                { key = "y", action = "copy_name" },
                { key = "Y", action = "copy_path" },
                { key = ",y", action = "copy_absolute_path" },
                { key = "[c", action = "prev_git_item" },
                { key = "]c", action = "next_git_item" },
                { key = "gx", action = "system_open" },
                { key = "f", action = "live_filter" },
                { key = "F", action = "clear_live_filter" },
                { key = { "q", "<esc>" }, action = "close" },
                { key = "K", action = "toggle_file_info" },
                { key = "?", action = "toggle_help" },
                { key = "-", action = "git_add", action_cb = git_add },
            },
        },
    },
    auto_reload_on_write = false,
    reload_on_bufenter = true,
    renderer = {
        highlight_git = true,
        highlight_opened_files = "icon",
        special_files = { "Cargo.toml", "Makefile", "README.md", "readme.md", "Project.toml" },
        icons = {
            glyphs = {
                git = {
                    unstaged = "",
                },
            },
        },
    },
    diagnostics = {
        enable = true,
        show_on_dirs = true,
        icons = {
            hint = "",
            info = "",
            warning = "",
            error = "",
        },
    },
})
