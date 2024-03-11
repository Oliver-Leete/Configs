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

local highlight = {
    "Rainbow1",
    "Rainbow2",
    "Rainbow3",
    "Rainbow4",
    "Rainbow5",
    "Rainbow6",
}

local hooks = require("ibl.hooks")

hooks.register(hooks.type.HIGHLIGHT_SETUP, function()
    vim.api.nvim_set_hl(0, "Rainbow1", { fg = Pt.dragonBlue })
    vim.api.nvim_set_hl(0, "Rainbow2", { fg = Pt.carpYellow })
    vim.api.nvim_set_hl(0, "Rainbow3", { fg = Pt.oniViolet })
    vim.api.nvim_set_hl(0, "Rainbow4", { fg = Pt.surimiOrange })
    vim.api.nvim_set_hl(0, "Rainbow5", { fg = Pt.autumnRed })
    vim.api.nvim_set_hl(0, "Rainbow6", { fg = Pt.waveAqua1 })
end)

local rainbow_delimiters = require("rainbow-delimiters")
vim.g.rainbow_delimiters = {
    highlight = highlight,
    query = {
        [''] = 'rainbow-delimiters',
        lua = 'rainbow-blocks',
        latex = 'rainbow-blocks',
    },
}
require("ibl").setup({
    indent = { char = " " },
    scope = {
        char = "▎",
        highlight = highlight,
    },
})
hooks.register(hooks.type.SCOPE_HIGHLIGHT, hooks.builtin.scope_highlight_from_extmark)
