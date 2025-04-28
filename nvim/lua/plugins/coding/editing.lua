---@module "lazy"
---@type LazySpec
return {
    {
        "folke/which-key.nvim",
        optional = true,
        opts = {
            to_add = {
                editing = {
                    { ",", group = "Editing" },

                    { ",t", icon = "󱇂 ", desc = "Alignment" },
                    { ",s", icon = "󰒺 ", desc = "Sort" },
                    { ",=", icon = " ", desc = "Evaluate" },
                    { ",n", icon = "󱘎 " },
                },
            },
        },
    },
    {
        "echasnovski/mini.operators",
        opts = {
            evaluate = { prefix = ",=" },
            exchange = {
                prefix = "$",
                reindent_linewise = false,
            },
            multiply = {
                prefix = "+",
            },
            replace = {
                prefix = "R",
                reindent_linewise = false,
            },
            sort = {
                prefix = ",s",
            },
        },
    },
    {
        "echasnovski/mini.align",
        opts = {
            mappings = {
                start = "",
                start_with_preview = ",t",
            },
        },
    },
    {
        "echasnovski/mini.move",
        opts = {},
    },
    { "ap/vim-you-keep-using-that-word" },
    {
        "kana/vim-niceblock",
        keys = {
            { "I", "<Plug>(niceblock-I)", mode = { "x" } },
            { "A", "<Plug>(niceblock-A)", mode = { "x" } },
        },
    },
    {
        "junegunn/vim-slash",
        dev = true,
    },
    {
        "wurli/contextindent.nvim",
        opts = { pattern = "*" },
        dependencies = { "nvim-treesitter/nvim-treesitter" },
    },
}
