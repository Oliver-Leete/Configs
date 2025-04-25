---@module "lazy"
---@type LazySpec
return {
    "folke/which-key.nvim",
    event = { "VeryLazy" },
    opts_extend = {  },
    opts = {
        preset = "helix",
        triggers = {
            { "<auto>", mode = "nxso" },
            { "v", mode = { "n", "x" } },
        },
        sort = {
            "alphanum",
        },
        plugins = {
            presets = {
                nav = false,
            },
        },
        to_add = {
            defaults = {
                { "?", group = "Information", icon = " " },

                { "?C", icon = " " },
                { "?N", icon = " " },
                { "?L", "<cmd>Lazy<cr>", desc = "Lazy info" },
                { "?S", "<cmd>LspInfo<cr>", desc = "Lsp info", icon = " " },
                { "?M", "<cmd>Mason<cr>", desc = "Mason info", icon = " " },
                { "?T", group = "Treesitter", icon = " " },
                { "?TM", "<cmd>TSModuleInfo<cr>", desc = "Treesitter module info", icon = "󰕳 " },
                { "?TC", "<cmd>TSConfigInfo<cr>", desc = "Treesitter config info", icon = " " },
                { "?TI", "<cmd>TSInstallInfo<cr>", desc = "Treesitter install info", icon = " " },

                { "<leader>d", group = "Debugger", icon = " " },
                { "<leader>g", group = "Git" },
                { "<leader>gd", group = "Diffview" },
                { "<leader>n", group = "Tasks", icon = " " },
                { "<leader>r", group = "Repl", icon = " " },
                { "<leader>s", group = "UI" },
                { "<leader>t", group = "Terminals", icon = " " },
                { "<leader>u", group = "Tests" },
                { "<leader>x", group = "Compiler Explorer", icon = " " },
                { "<leader>z", group = "Toggles" },

                { "<leader>f", icon = "󰈞 " },

                { "<leader>P", icon = " " },
                { "<leader>p", icon = " " },
                { "<leader>ps", icon = " " },
                { "<leader>pp", icon = " " },
                { "<leader>pS", icon = " " },
                { "<leader>pw", icon = "󰑑 " },
                { "<leader>pg", group = "Git pickers" },
                { "<leader>po", group = "Option pickers" },

                { ",", group = "Editing" },

                { ",t", icon = "󱇂 ", desc = "Alignment" },
                { ",s", icon = "󰒺 ", desc = "Sort" },
                { ",=", icon = " ", desc = "Evaluate" },
                { ",n", icon = "󱘎 " },
            },
        },
    },
    config = function(_, opts)
        local wk = require("which-key")
        local to_add = opts.to_add
        opts.to_add = nil
        wk.setup(opts)
        for _, group in pairs(to_add) do
            wk.add(group)
        end
    end,
}
