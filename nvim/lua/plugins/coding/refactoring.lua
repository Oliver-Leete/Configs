---@module "lazy"
---@type LazySpec
return {
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
}
