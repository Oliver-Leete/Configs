---@module "lazy"
---@type LazySpec
return {
    {
        "folke/snacks.nvim",
        opts = {
            picker = {
                layout = { preset = "ivy" },
                matcher = {
                    frecency = true,
                },
                win = {
                    input = {
                        keys = {
                            ["<c-x>"] = { "edit_split", mode = { "n", "i" } },
                            ["<c-u>"] = "",
                            ["<c-a>"] = "",
                            ["<c-space>"] = { "toggle_live", mode = { "n", "i" } },
                        },
                    },
                },
            },
        },
        keys = {
            { "<leader>f", function() require("snacks.picker").files({ hidden = true }) end, desc = "Find files" },

            {
                "<leader>P",
                function() require("snacks.picker").resume() end,
                desc = "Resume last picker",
            },
            { "<leader>p/", function() require("snacks.picker").lines() end, desc = "Buffer Lines" },
            {
                "<leader>p:",
                function() require("snacks.picker").command_history() end,
                desc = "Command History",
            },
            {
                "<leader>p?",
                function() require("snacks.picker").grep_buffers() end,
                desc = "Grep Open Buffers",
            },
            { "<leader>pb", function() require("snacks.picker").buffers() end, desc = "Buffers" },
            { "<leader>pd", function() require("snacks.picker").diagnostics() end, desc = "Diagnostics" },
            {
                "<leader>pD",
                function() require("snacks.picker").diagnostics_buffer() end,
                desc = "Buffer Diagnostics",
            },
            {
                "<leader>pl",
                function() require("snacks.picker").lazy() end,
                desc = "Search for Plugin Spec",
            },
            {
                "<leader>pn",
                function() require("snacks.picker").notifications() end,
                desc = "Notification History",
            },
            { "<leader>pp", function() require("snacks.picker").projects() end, desc = "Projects" },
            { "<leader>pr", function() require("snacks.picker").recent() end, desc = "Recent" },
            {
                "<leader>ps",
                function() require("snacks.picker").lsp_workspace_symbols() end,
                desc = "Workspace symbols",
            },
            { "<leader>pS", function() require("snacks.picker").lsp_symbols() end, desc = "LSP Symbols" },
            { "<leader>pu", function() require("snacks.picker").undo() end, desc = "Undo History" },
            { "<leader>pw", function() require("snacks.picker").grep() end, desc = "Grep" },

            { "<leader>pgb", function() require("snacks.picker").git_branches() end, desc = "Git Branches" },
            {
                "<leader>pgd",
                function() require("snacks.picker").git_diff() end,
                desc = "Git Diff (Hunks)",
            },
            { "<leader>pgf", function() require("snacks.picker").git_log_file() end, desc = "Git Log File" },
            { "<leader>pgl", function() require("snacks.picker").git_log() end, desc = "Git Log" },
            { "<leader>pgL", function() require("snacks.picker").git_log_line() end, desc = "Git Log Line" },
            { "<leader>pgs", function() require("snacks.picker").git_status() end, desc = "Git Status" },
            { "<leader>pgS", function() require("snacks.picker").git_stash() end, desc = "Git Stash" },

            { '<leader>po"', function() require("snacks.picker").registers() end, desc = "Registers" },
            {
                "<leader>pos",
                function() require("snacks.picker").search_history() end,
                desc = "Search History",
            },
            { "<leader>poa", function() require("snacks.picker").autocmds() end, desc = "Autocmds" },
            { "<leader>poc", function() require("snacks.picker").commands() end, desc = "Commands" },
            { "<leader>poh", function() require("snacks.picker").help() end, desc = "Help Pages" },
            { "<leader>poH", function() require("snacks.picker").highlights() end, desc = "Highlights" },
            { "<leader>poi", function() require("snacks.picker").icons() end, desc = "Icons" },
            { "<leader>poj", function() require("snacks.picker").jumps() end, desc = "Jumps" },
            { "<leader>pok", function() require("snacks.picker").keymaps() end, desc = "Keymaps" },
            { "<leader>pom", function() require("snacks.picker").marks() end, desc = "Marks" },
            { "<leader>poM", function() require("snacks.picker").man() end, desc = "Man Pages" },
            { "<leader>poS", function() require("snacks.picker").colorschemes() end, desc = "Colorschemes" },
        },
    },
    {
        "folke/todo-comments.nvim",
        optional = true,
        keys = {
            { "<leader>pt", function() Snacks.picker.todo_comments() end, desc = "Todo" }, ---@diagnostic disable-line: undefined-field
        },
    },
}
