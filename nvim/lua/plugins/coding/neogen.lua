---@module "lazy"
---@type LazySpec
return {
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
}
