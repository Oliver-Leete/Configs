---@module "lazy"
---@type LazySpec
return {
    {
        "folke/which-key.nvim",
        optional = true,
        opts = {
            to_add = {
                annotating = {
                    { ",aa", group = "Annotating", icon = " " },
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
            { ",aa", function() require("neogen").generate() end, desc = "Annotate thing" },
            { ",af", function() require("neogen").generate({ type = "func" }) end, desc = "Annotate function" },
            { ",ac", function() require("neogen").generate({ type = "class" }) end, desc = "Annotate class" },
            { ",at", function() require("neogen").generate({ type = "type" }) end, desc = "Annotate type" },
        },
    },
    {
        "Davidyz/inlayhint-filler.nvim",
        keys = {
            {
                ",ai",
                function() require("inlayhint-filler").fill() end,
                desc = "Fill inlay hint.",
                mode = { "n", "v" },
            },
        },
    },
}
