---@module "lazy"
---@type LazySpec
return {
    "Davidyz/inlayhint-filler.nvim",
    keys = {
        { ",i", function() require("inlayhint-filler").fill() end, desc = "Fill inlay hint.", mode = { "n", "v" } },
    },
}
