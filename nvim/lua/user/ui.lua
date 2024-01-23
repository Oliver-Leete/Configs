vim.opt.termguicolors = true
vim.opt.guifont = "JuliaMono:h10"

require("dressing").setup({
    select = {
        backend = { "telescope" },
        telescope = require("telescope.themes").get_ivy({
            height = 30,
        }),
        get_config = function(opts)
            if opts.kind == "codeaction" then
                return {
                    backend = "telescope",
                    telescope = require("telescope.themes").get_cursor({ layout_config = { height = 15 } })
                }
            else
                return {
                    backend = "telescope",
                    telescope = require("telescope.themes").get_dropdown()
                }
            end
        end
    },
})

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
            TelescopeTitle         = { fg = theme.ui.special, bold = true },
            TelescopePromptNormal  = { bg = theme.ui.bg_m1 },
            TelescopePromptBorder  = { fg = theme.ui.bg_p1, bg = theme.ui.bg_p1 },
            TelescopePromptPrefix  = { fg = theme.ui.special, bg = theme.ui.bg_p1 },
            TelescopePromptCounter = { fg = theme.ui.special, bg = theme.ui.bg_p1 },
            TelescopeResultsNormal = { fg = theme.ui.fg_dim, bg = theme.ui.bg_m1 },
            TelescopeResultsBorder = { fg = theme.ui.bg_m1, bg = theme.ui.bg_m1 },
            TelescopePreviewNormal = { bg = theme.ui.bg_dim },
            TelescopePreviewBorder = { bg = theme.ui.bg_dim, fg = theme.ui.bg_dim },
        }
    end,
})

vim.cmd("colorscheme kanagawa")
Ct = require("kanagawa.colors").setup({ colors = kcolors }).theme

vim.api.nvim_set_hl(0, "SubstituteExchange", { link = "MatchParen" })


vim.api.nvim_set_hl(0, "LineNr", { fg = Ct.ui.bg_p2, bg = Ct.ui.bg, bold = false })
vim.api.nvim_set_hl(0, "LineSep", { fg = Ct.ui.bg_p2, bg = Ct.ui.bg, bold = false })
vim.api.nvim_set_hl(0, "CursorLineNr", { fg = Ct.ui.fg, bg = Ct.ui.bg_p2, bold = true })
vim.api.nvim_set_hl(0, "CursorLineSep", { fg = Ct.ui.bg_p2, bg = Ct.ui.bg_p2, bold = true })

vim.api.nvim_set_hl(0, "StatusLineNC", { fg = Ct.ui.bg_p2, bg = Ct.ui.bg })

vim.g.matchup_matchparen_deferred = true
vim.g.matchup_matchparen_hi_surround_always = true

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
    no_exec_files = { "packer", "TelescopePrompt", "mason", "CompetiTest", "NvimTree" },
    symbols = {
        "━",
        "┃",
        "┏",
        "┓",
        "┗",
        "┛",
    },
})

local glance = require('glance')
local actions = glance.actions

vim.api.nvim_set_hl(0, "GlanceBorderTop", { fg = Ct.syn.fun, bg = "#2f2f39" })
vim.api.nvim_set_hl(0, "GlanceListBorderBottom", { fg = Ct.syn.fun, bg = "#2f2f39" })
vim.api.nvim_set_hl(0, "GlancePreviewBorderBottom", { fg = Ct.syn.fun, bg = "#2a2933" })

glance.setup({
    height = 20,
    border = {
        enable = false,
        top_char = '━',
        bottom_char = '━',
    },
    mappings = {
        list = {
            ['j'] = actions.next,
            ['k'] = actions.previous,
            ['<Down>'] = actions.next,
            ['<Up>'] = actions.previous,
            ['n'] = actions.next_location,
            ['N'] = actions.previous_location,
            ['<C-k>'] = actions.preview_scroll_win(5),
            ['<C-j>'] = actions.preview_scroll_win(-5),
            ['<c-v>'] = actions.jump_vsplit,
            ['<c-x>'] = actions.jump_split,
            ['<CR>'] = actions.jump,
            ['o'] = actions.enter_win('preview'),
            ['p'] = actions.enter_win('preview'),
            ["<C-l>"] = function()
                actions.quickfix()
                vim.cmd.cclose()
            end,
            ['<Esc>'] = actions.close,
        },
        preview = {
            ['n'] = actions.next_location,
            ['N'] = actions.previous_location,
            ['<esc>'] = actions.enter_win('list'),
            ['<leader><cr>'] = actions.enter_win('list'),
        },
    },
    folds = {
        fold_closed = "",
        fold_open = "",
        folded = true,
    },
})

vim.go.statuscolumn = "%{%v:lua.StatusCol()%}"
vim.wo.statuscolumn = "%{%v:lua.StatusCol()%}"

local statcol = vim.api.nvim_create_augroup("StatusCol", {})

vim.api.nvim_set_hl(0, "PmenuSel", { bg = Ct.ui.bg, fg = "NONE" })
vim.api.nvim_set_hl(0, "Pmenu", { fg = "#C5CDD9", bg = Ct.ui.bg })

vim.api.nvim_set_hl(0, "CmpItemAbbrDeprecated", { fg = "#7E8294", bg = "NONE", strikethrough = true })
vim.api.nvim_set_hl(0, "CmpItemAbbrMatch", { fg = "#82AAFF", bg = "NONE", bold = true })
vim.api.nvim_set_hl(0, "CmpItemAbbrMatchFuzzy", { fg = "#82AAFF", bg = "NONE", bold = true })
vim.api.nvim_set_hl(0, "CmpItemMenu", { fg = "#C792EA", bg = "NONE", italic = true })

vim.api.nvim_set_hl(0, "CmpItemKindField", { fg = "#EED8DA", bg = "#B5585F" })
vim.api.nvim_set_hl(0, "CmpItemKindProperty", { fg = "#EED8DA", bg = "#B5585F" })
vim.api.nvim_set_hl(0, "CmpItemKindEvent", { fg = "#EED8DA", bg = "#B5585F" })

vim.api.nvim_set_hl(0, "CmpItemKindText", { fg = "#C3E88D", bg = "#9FBD73" })
vim.api.nvim_set_hl(0, "CmpItemKindEnum", { fg = "#C3E88D", bg = "#9FBD73" })
vim.api.nvim_set_hl(0, "CmpItemKindKeyword", { fg = "#C3E88D", bg = "#9FBD73" })

vim.api.nvim_set_hl(0, "CmpItemKindConstant", { fg = "#FFE082", bg = "#D4BB6C" })
vim.api.nvim_set_hl(0, "CmpItemKindConstructor", { fg = "#FFE082", bg = "#D4BB6C" })
vim.api.nvim_set_hl(0, "CmpItemKindReference", { fg = "#FFE082", bg = "#D4BB6C" })

vim.api.nvim_set_hl(0, "CmpItemKindFunction", { fg = "#EADFF0", bg = "#A377BF" })
vim.api.nvim_set_hl(0, "CmpItemKindStruct", { fg = "#EADFF0", bg = "#A377BF" })
vim.api.nvim_set_hl(0, "CmpItemKindClass", { fg = "#EADFF0", bg = "#A377BF" })
vim.api.nvim_set_hl(0, "CmpItemKindModule", { fg = "#EADFF0", bg = "#A377BF" })
vim.api.nvim_set_hl(0, "CmpItemKindOperator", { fg = "#EADFF0", bg = "#A377BF" })

vim.api.nvim_set_hl(0, "CmpItemKindVariable", { fg = "#C5CDD9", bg = "#7E8294" })
vim.api.nvim_set_hl(0, "CmpItemKindFile", { fg = "#C5CDD9", bg = "#7E8294" })

vim.api.nvim_set_hl(0, "CmpItemKindUnit", { fg = "#F5EBD9", bg = "#D4A959" })
vim.api.nvim_set_hl(0, "CmpItemKindSnippet", { fg = "#F5EBD9", bg = "#D4A959" })
vim.api.nvim_set_hl(0, "CmpItemKindFolder", { fg = "#F5EBD9", bg = "#D4A959" })

vim.api.nvim_set_hl(0, "CmpItemKindMethod", { fg = "#DDE5F5", bg = "#6C8ED4" })
vim.api.nvim_set_hl(0, "CmpItemKindValue", { fg = "#DDE5F5", bg = "#6C8ED4" })
vim.api.nvim_set_hl(0, "CmpItemKindEnumMember", { fg = "#DDE5F5", bg = "#6C8ED4" })

vim.api.nvim_set_hl(0, "CmpItemKindInterface", { fg = "#D8EEEB", bg = "#58B5A8" })
vim.api.nvim_set_hl(0, "CmpItemKindColor", { fg = "#D8EEEB", bg = "#58B5A8" })
vim.api.nvim_set_hl(0, "CmpItemKindTypeParameter", { fg = "#D8EEEB", bg = "#58B5A8" })
