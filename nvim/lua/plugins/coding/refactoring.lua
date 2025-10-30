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
