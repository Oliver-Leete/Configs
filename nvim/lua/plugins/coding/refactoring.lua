---@module "lazy"
---@type LazySpec
return {
    {
        "folke/which-key.nvim",
        optional = true,
        opts = {
            to_add = {
                refactoring = {
                    { ",r", group = "Refactoring" },
                },
            },
        },
    },
    {
        "ThePrimeagen/refactoring.nvim",
        dependencies = {
            "nvim-lua/plenary.nvim",
            "nvim-treesitter/nvim-treesitter",
        },
        lazy = false,
        opts = {},
        keys = {
            { ",rf", ":Refactor extract ", desc = "Extract function", mode = { "x" } },
            { ",rF", ":Refactor extract_to_file", desc = "Extract function to file", mode = { "x" } },
            { ",rI", ":Refactor inline_func", desc = "Inline function" },

            { ",re", ":Refactor extract_var ", desc = "Extract variable", mode = { "x" } },
            { ",ri", ":Refactor inline_var", desc = "Inline variable", mode = { "n", "x" } },

            { ",rb", ":Refactor extract_block", desc = "Extract block" },
            { ",rB", ":Refactor extract_block_to_file", desc = "Extract block to file" },
        },
    },
    {
        "SleepySwords/change-function.nvim",
        dependencies = {
            "MunifTanjim/nui.nvim",
            "nvim-treesitter/nvim-treesitter",
            "nvim-treesitter/nvim-treesitter-textobjects",
        },
        keys = {
            { ",ra", function() require("change-function").change_function() end, desc = "Change function signature" },
        },
    },
    {
        "chrisgrieser/nvim-genghis",
        optional = true,
        keys = {
            {
                ",rn",
                function() require("genghis").moveSelectionToNewFile() end,
                desc = "Extract to a new file",
                mode = { "x" },
            },
        },
    },
    {
        "chrisgrieser/nvim-rip-substitute",
        cmd = "RipSubstitute",
        opts = {},
        keys = {
            { ",rs", function() require("rip-substitute").sub() end, mode = { "n", "x" }, desc = "Rip substitute" },
        },
    },
}
