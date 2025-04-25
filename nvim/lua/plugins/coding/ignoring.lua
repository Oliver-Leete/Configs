---@module "lazy"
---@type LazySpec
return {
    {
        "folke/which-key.nvim",
        optional = true,
        opts = {
            to_add = {
                ignoring = {
                    { ",i", group = "Ignoring", icon = " " },
                },
            },
        },
    },
    {
        "chrisgrieser/nvim-rulebook",
        opts = {},
        cmd = { "Rulebook" },
        keys = {
            { ",ii", function() require("rulebook").ignoreRule() end, desc = "Ignore rule" },
            { ",iw", function() require("rulebook").lookupRule() end, desc = "Lookup rule" },
            { ",iy", function() require("rulebook").yankDiagnosticCode() end, desc = "Yank rule code" },
            {
                ",if",
                function() require("rulebook").suppressFormatter() end,
                desc = "Yank rule code",
                mode = { "n", "x" },
            },
        },
    },
}
