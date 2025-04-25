---@module "lazy"
---@type LazySpec
return {
    {
        "folke/which-key.nvim",
        optional = true,
        opts = {
            to_add = {
                annotating = {
                    { ",a", icon = " " },
                },
            },
        },
    },
    {
        "danymat/neogen",
        dependencies = { "L3MON4D3/LuaSnip" },
        opts = {
            snippet_engine = "luasnip",
            languages = {
                lua = { template = { annotation_convention = "emmylua" } },
                python = { template = { annotation_convention = "reST" } },
            },
        },
        cmd = {
            "Neogen",
        },
        keys = {
            { ",a", function() require("neogen").generate() end, desc = "Make annotation" },
        },
    },
}
