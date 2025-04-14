---@module "lazy"
---@type LazySpec
return {
    'mrjones2014/legendary.nvim',
    dependencies = {
        "folke/which-key.nvim",
    },
    opts = {
        funcs = {
            { function() vim.cmd("left @/=''") end,              description = "Clear search" },
            { function() vim.cmd.tabclose() end,                 description = "Close tab" },
            { function() require("mini.bufremove").delete() end, description = "Delete buffer" },

            {
                itemgroup = "Pickers",
                icon = " ",
                funcs = {
                    { function() require("snacks.picker").buffers() end,               description = "Buffers" },
                    { function() require("snacks.picker").grep() end,                  description = "Grep" },
                    { function() require("snacks.picker").command_history() end,       description = "Command History" },
                    { function() require("snacks.picker").files() end,                 description = "Find Files" },
                    { function() require("snacks.picker").buffers() end,               description = "Buffers" },
                    { function() require("snacks.picker").files() end,                 description = "Find Files" },
                    { function() require("snacks.picker").git_files() end,             description = "Find Git Files" },
                    { function() require("snacks.picker").recent() end,                description = "Recent" },
                    { function() require("snacks.picker").git_log() end,               description = "Git Log" },
                    { function() require("snacks.picker").git_status() end,            description = "Git Status" },
                    { function() require("snacks.picker").lines() end,                 description = "Buffer Lines" },
                    { function() require("snacks.picker").grep_buffers() end,          description = "Grep Open Buffers" },
                    { function() require("snacks.picker").grep() end,                  description = "Grep" },
                    { function() require("snacks.picker").registers() end,             description = "Registers" },
                    { function() require("snacks.picker").autocmds() end,              description = "Autocmds" },
                    { function() require("snacks.picker").command_history() end,       description = "Command History" },
                    { function() require("snacks.picker").commands() end,              description = "Commands" },
                    { function() require("snacks.picker").diagnostics() end,           description = "Diagnostics" },
                    { function() require("snacks.picker").help() end,                  description = "Help Pages" },
                    { function() require("snacks.picker").highlights() end,            description = "Highlights" },
                    { function() require("snacks.picker").jumps() end,                 description = "Jumps" },
                    { function() require("snacks.picker").keymaps() end,               description = "Keymaps" },
                    { function() require("snacks.picker").loclist() end,               description = "Location List" },
                    { function() require("snacks.picker").man() end,                   description = "Man Pages" },
                    { function() require("snacks.picker").marks() end,                 description = "Marks" },
                    { function() require("snacks.picker").resume() end,                description = "Resume" },
                    { function() require("snacks.picker").qflist() end,                description = "Quickfix List" },
                    { function() require("snacks.picker").colorschemes() end,          description = "Colorschemes" },
                    { function() require("snacks.picker").projects() end,              description = "Projects" },
                    { function() require("snacks.picker").lsp_workspace_symbols() end, description = "Workspace Symbols" }
                },
            },
        },
        icons = {
            keymap = " ",
            command = ' ',
            fn = '󰡱 ',
            itemgroup = ' ',
        },
        include_builtin = false,
        include_legendary_cmds = false,
        lazy_nvim = {
            auto_register = true,
        },
        which_key = {
            auto_register = true,
            do_binding = false,
            use_groups = true,
        },
        extensions = {
            lazy_nvim = true,
            diffview = true,
        },
    },
    keys = {
        { "<leader>p", function() require("legendary").find() end, desc = "Commands", },
    }

}
