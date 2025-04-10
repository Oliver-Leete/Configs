---@module "lazy"
---@type LazySpec
return {
    "Davidyz/inlayhint-filler.nvim",
    keys = {
        { "<c-m>", function() require("inlayhint-filler").fill() end, desc = "Insert the current inlay-hint.", mode = { "n", "v" } },
    }
}
