local cmd_setup = function()
    -- Command Panel Bindings

    local commander = require("commander")
    commander.setup({
        integration = {
            telescope = {
                enable = false,
            },
        },
        components = { "CAT", "DESC", "KEYS", },
        sort_by = { "CAT", "DESC", "CMD", "KEYS", },
    })

    commander.add({
        { cat = "default", desc = "Clear search",        cmd = "<cmd>let @/=''<cr>" },
        { cat = "tab",     desc = "Close tab",           cmd = "<cmd>tabclose<cr>" },
        { cat = "buffer",  desc = "Delete buffer",       cmd = function() require("mini.bufremove").delete() end },
        { cat = "info",    desc = "Lazy",                cmd = "<cmd>Lazy<cr>" },
        { cat = "info",    desc = "Mason",               cmd = "<cmd>Mason<cr>" },
        { cat = "info",    desc = "Lsp info",            cmd = "<cmd>LspInfo<cr>" },
        { cat = "info",    desc = "None-ls info",        cmd = "<cmd>NullLsInfo<cr>" },
        { cat = "file",    desc = "New file",            cmd = function() require("genghis").createNewFile() end },
        { cat = "file",    desc = "Rename file",         cmd = function() require("genghis").renameFile() end },
        { cat = "file",    desc = "Move file",           cmd = function() require("genghis").moveAndRenameFile() end },
        { cat = "file",    desc = "Copy file",           cmd = function() require("genghis").duplicateFile() end },
        { cat = "file",    desc = "Move file to subdir", cmd = function() require("genghis").moveToFolderInCwd() end },
        { cat = "file",    desc = "Trash file",          cmd = function() require("genghis").trashFile() end },
        { cat = "diff",    desc = "Diff of unstaged",    cmd = "<cmd>DiffviewOpen<cr>" },
        { cat = "diff",    desc = "File diff history",   cmd = "<cmd>DiffviewFileHistory %<cr>" },
        { cat = "diff",    desc = "Folder diff history", cmd = "<cmd>DiffviewFileHistory<cr>" },
        { cat = "dap",     desc = "Run Debugger",        cmd = "<cmd>DapContinue<cr>" },

        { cat = "finders", desc = "Buffers",             cmd = function() require("snacks.picker").buffers() end },
        { cat = "finders", desc = "Grep",                cmd = function() require("snacks.picker").grep() end },
        { cat = "finders", desc = "Command History",     cmd = function() require("snacks.picker").command_history() end },
        { cat = "finders", desc = "Find Files",          cmd = function() require("snacks.picker").files() end },
        { cat = "finders", desc = "Buffers",             cmd = function() require("snacks.picker").buffers() end },
        { cat = "finders", desc = "Find Files",          cmd = function() require("snacks.picker").files() end },
        { cat = "finders", desc = "Find Git Files",      cmd = function() require("snacks.picker").git_files() end },
        { cat = "finders", desc = "Recent",              cmd = function() require("snacks.picker").recent() end },
        { cat = "finders", desc = "Git Log",             cmd = function() require("snacks.picker").git_log() end },
        { cat = "finders", desc = "Git Status",          cmd = function() require("snacks.picker").git_status() end },
        { cat = "finders", desc = "Buffer Lines",        cmd = function() require("snacks.picker").lines() end },
        { cat = "finders", desc = "Grep Open Buffers",   cmd = function() require("snacks.picker").grep_buffers() end },
        { cat = "finders", desc = "Grep",                cmd = function() require("snacks.picker").grep() end },
        { cat = "finders", desc = "Registers",           cmd = function() require("snacks.picker").registers() end },
        { cat = "finders", desc = "Autocmds",            cmd = function() require("snacks.picker").autocmds() end },
        { cat = "finders", desc = "Command History",     cmd = function() require("snacks.picker").command_history() end },
        { cat = "finders", desc = "Commands",            cmd = function() require("snacks.picker").commands() end },
        { cat = "finders", desc = "Diagnostics",         cmd = function() require("snacks.picker").diagnostics() end },
        { cat = "finders", desc = "Help Pages",          cmd = function() require("snacks.picker").help() end },
        { cat = "finders", desc = "Highlights",          cmd = function() require("snacks.picker").highlights() end },
        { cat = "finders", desc = "Jumps",               cmd = function() require("snacks.picker").jumps() end },
        { cat = "finders", desc = "Keymaps",             cmd = function() require("snacks.picker").keymaps() end },
        { cat = "finders", desc = "Location List",       cmd = function() require("snacks.picker").loclist() end },
        { cat = "finders", desc = "Man Pages",           cmd = function() require("snacks.picker").man() end },
        { cat = "finders", desc = "Marks",               cmd = function() require("snacks.picker").marks() end },
        { cat = "finders", desc = "Resume",              cmd = function() require("snacks.picker").resume() end },
        { cat = "finders", desc = "Quickfix List",       cmd = function() require("snacks.picker").qflist() end },
        { cat = "finders", desc = "Colorschemes",        cmd = function() require("snacks.picker").colorschemes() end },
        { cat = "finders", desc = "Projects",            cmd = function() require("snacks.picker").projects() end },
        {
            cat = "finders",
            desc = "Workspace Symbols",
            cmd = function()
                require("snacks.picker")
                    .lsp_workspace_symbols()
            end
        },

    })
end

return {
    "FeiyouG/commander.nvim",
    config = cmd_setup,
    keys = {
        { "<leader>p", function() require("commander").show() end, desc = "Commands" }
    },
    dependencies = {
        { "folke/snacks.nvim" },
        { "nvim-telescope/telescope.nvim" },
    },
}
