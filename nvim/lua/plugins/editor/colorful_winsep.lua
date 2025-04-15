---@module "lazy"
---@type LazySpec
return {
    "nvim-zh/colorful-winsep.nvim",
    opts = {
        hi = { link = "Function" },
        symbols = { "─", "│", "╭", "╮", "╰", "╯" },
        smooth = false,
    },
}
