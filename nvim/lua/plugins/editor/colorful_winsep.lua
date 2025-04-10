---@module "lazy"
---@type LazySpec
return {
    "nvim-zh/colorful-winsep.nvim",
    dev = true,
    init = function()
        vim.api.nvim_set_hl(0, "NvimSeparator", { link = "Function" })
    end,
    opts = {
        symbols = { '─', '│', '╭', '╮', '╰', '╯' },
        smooth = false,
    },
}
