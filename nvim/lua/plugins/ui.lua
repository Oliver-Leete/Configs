local ui_setup = function()
    vim.opt.termguicolors = true
    vim.opt.guifont = "JuliaMono:h10"

    local kcolors = {
        theme = {
            all = {
                ui = {
                    bg_gutter = "none",
                    bg_m3 = "none",
                }
            }
        }
    }

    require("kanagawa").setup({
        background = { light = "lotus", dark = "wave" },
        keywordStyle = { italic = false },
        undercurl = false,
        statementStyle = { italic = false, bold = false },
        variablebuiltinStyle = { italic = false },
        colors = kcolors,
        overrides = function(colors)
            local theme = colors.theme
            return {
                WinSeparator = { bg = theme.ui.bg, fg = theme.ui.bg_m2 },
            }
        end,
    })

    vim.cmd("colorscheme kanagawa")
    Ct = require("kanagawa.colors").setup({ colors = kcolors }).theme

    local colorful_winsep = require("colorful-winsep")
    colorful_winsep.setup({
        highlight = { link = "Function" },
        interval = 30,
        no_exec_files = { "mason", },
        symbols = { '─', '│', '╭', '╮', '╰', '╯' },
        smooth = false,
    })
end

return {
    "rebelot/kanagawa.nvim",
    dependencies = {
        { "nvim-zh/colorful-winsep.nvim" },
    },
    config = ui_setup,
}
