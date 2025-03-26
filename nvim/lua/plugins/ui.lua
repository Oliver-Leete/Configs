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
                Pmenu                  = { fg = theme.ui.shade0, bg = theme.ui.bg_p1 },
                PmenuSel               = { fg = "NONE", bg = theme.ui.bg_p2 },
                PmenuSbar              = { bg = theme.ui.bg_m1 },
                PmenuThumb             = { bg = theme.ui.bg_p2 },
                WinSeparator           = { bg = theme.ui.bg, fg = theme.ui.bg_m2 },
            }
        end,
    })

    vim.cmd("colorscheme kanagawa")
    Ct = require("kanagawa.colors").setup({ colors = kcolors }).theme
    Pt = require("kanagawa.colors").setup({ colors = kcolors }).palette

    vim.api.nvim_set_hl(0, "SubstituteExchange", { link = "MatchParen" })


    vim.api.nvim_set_hl(0, "LineNr", { fg = Ct.ui.bg_p2, bg = Ct.ui.bg, bold = false })
    vim.api.nvim_set_hl(0, "LineSep", { fg = Ct.ui.bg_p2, bg = Ct.ui.bg, bold = false })
    vim.api.nvim_set_hl(0, "CursorLineNr", { fg = Ct.ui.fg, bg = Ct.ui.bg_p2, bold = true })
    vim.api.nvim_set_hl(0, "CursorLineSep", { fg = Ct.ui.bg_p2, bg = Ct.ui.bg_p2, bold = true })

    vim.api.nvim_set_hl(0, "StatusLineNC", { fg = Ct.ui.bg_p2, bg = Ct.ui.bg })

    vim.api.nvim_set_hl(0, "TabLine", { fg = Ct.ui.bg, bg = Ct.syn.comment })
    vim.api.nvim_set_hl(0, "TabLineMids", { fg = Ct.ui.bg, bg = Ct.syn.comment })
    vim.api.nvim_set_hl(0, "TabLineEnds", { fg = Ct.syn.comment, bg = Ct.ui.bg })
    vim.api.nvim_set_hl(0, "TabLineActive", { fg = Ct.ui.bg, bg = Ct.syn.fun })
    vim.api.nvim_set_hl(0, "TabLineActiveMids", { fg = Ct.syn.fun, bg = Ct.syn.comment })
    vim.api.nvim_set_hl(0, "TabLineActiveEnds", { fg = Ct.syn.fun, bg = Ct.ui.bg })
    vim.api.nvim_set_hl(0, "TabLineBlank", { fg = Ct.ui.bg, bg = Ct.ui.bg })

    local colorful_winsep = require("colorful-winsep")
    colorful_winsep.setup({
        highlight = {
            bg = Ct.ui.bg,
            fg = Ct.syn.fun,
        },
        interval = 30,
        no_exec_files = { "packer", "mason", "CompetiTest", "NvimTree" },
        symbols = {
            "━",
            "┃",
            "┏",
            "┓",
            "┗",
            "┛",
        },
    })
end

return {
    "rebelot/kanagawa.nvim",
    dependencies = {
        { "nvim-zh/colorful-winsep.nvim" },
    },
    config = ui_setup,
}
