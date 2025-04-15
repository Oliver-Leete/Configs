---@module "lazy"
---@type LazySpec
return {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000,
    opts = {
        styles = {
            sidebars = "normal",
            floats = "normal",
        },
    },
    config = function(_, opts)
        require("tokyonight").setup(opts)
        vim.cmd.colorscheme("tokyonight")
    end,
}
