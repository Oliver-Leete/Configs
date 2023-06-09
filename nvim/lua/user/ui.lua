vim.opt.termguicolors = true
vim.opt.guifont = "JuliaMono:h10"

require("dressing").setup({
    select = {
        backend = { "telescope" },
        telescope = require("telescope.themes").get_ivy({
            height = 30,
        }),
        get_config = function(opts)
            Kind = opts.kind
            if opts.kind == "codeaction" then
                return {
                    backend = "telescope",
                    telescope = require("telescope.themes").get_cursor({})
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

require("kanagawa").setup({
    keywordStyle = { italic = false },
    statementStyle = { italic = false, bold = false },
    variablebuiltinStyle = { italic = false },
    colors = {
        theme = {
            all = {
                ui = {
                    bg_gutter = "none",
                    bg_m3 = "none",
                }
            }
        }
    },
    overrides = function(colors)
        local theme = colors.theme
        return {
            Pmenu = { fg = theme.ui.shade0, bg = theme.ui.bg_p1 },
            PmenuSel = { fg = "NONE", bg = theme.ui.bg_p2 },
            PmenuSbar = { bg = theme.ui.bg_m1 },
            PmenuThumb = { bg = theme.ui.bg_p2 },
            WinSeparator = { bg = theme.ui.bg, fg = theme.ui.bg_m2 },
        }
    end,
})

vim.cmd("colorscheme kanagawa")
Tc = require("kanagawa.colors").setup().palette
Ct = require("kanagawa.colors").setup().theme

vim.api.nvim_set_hl(0, "SubstituteExchange", { link = "MatchParen" })

local background = vim.api.nvim_get_hl_by_name("CursorLine", true).background

local sign_colours = { Add = "Added", Change = "Changed", Delete = "Deleted" }
for sign, colour in pairs(sign_colours) do
    local highlight = vim.api.nvim_get_hl_by_name("diff" .. colour, true)
    highlight.background = background
    vim.api.nvim_set_hl(0, "GitSigns" .. sign .. "Cul", highlight)
end

vim.api.nvim_set_hl(0, "LineNr", { fg = Tc.sumiInk5, bg = Tc.sumiInk3, bold = false })
vim.api.nvim_set_hl(0, "LineSep", { fg = Tc.sumiInk5, bg = Tc.sumiInk3, bold = false })
vim.api.nvim_set_hl(0, "CursorLineNr", { fg = Tc.fujiWhite, bg = Tc.sumiInk5, bold = true })
vim.api.nvim_set_hl(0, "CursorLineSep", { fg = Tc.sumiInk5, bg = Tc.sumiInk5, bold = true })

vim.api.nvim_set_hl(0, "StatusLineNC", { fg = Tc.sumiInk5, bg = Ct.ui.bg })

vim.g.matchup_matchparen_deferred = true
vim.g.matchup_matchparen_hi_surround_always = true

local CursorMatchParen = vim.api.nvim_get_hl_by_name("MatchParen", true)
CursorMatchParen.background = background
vim.api.nvim_set_hl(0, "CursorMatchParen", CursorMatchParen)

require("notify").setup({
    top_down = false,
})

vim.notify = require("notify")

vim.api.nvim_set_hl(0, "TabLine", { fg = Tc.bg, bg = Tc.fujiGray, sp = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "TabLineEnds", { fg = Tc.fujiGray, bg = Tc.bg, sp = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "TabLineActive", { fg = Tc.bg, bg = Tc.crystalBlue, sp = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "TabLineActiveEnds", { fg = Tc.crystalBlue, bg = Tc.bg, sp = Tc.sumiInk3 })

local colorful_winsep = require("colorful-winsep")
colorful_winsep.setup({
    highlight = {
        bg = Tc.bg,
        fg = Tc.crystalBlue
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

vim.api.nvim_set_hl(0, "GlanceBorderTop", { fg = Tc.crystalBlue, bg = "#2f2f39" })
vim.api.nvim_set_hl(0, "GlanceListBorderBottom", { fg = Tc.crystalBlue, bg = "#2f2f39" })
vim.api.nvim_set_hl(0, "GlancePreviewBorderBottom", { fg = Tc.crystalBlue, bg = "#2a2933" })

glance.setup({
    height = 20,
    border = {
        enable = true,
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
            ["<C-q>"] = actions.quickfix,
            ["<C-l>"] = actions.quickfix,
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

vim.go.statuscolumn = "%{%v:lua.Normal_StatusCol()%}"
vim.wo.statuscolumn = "%{%v:lua.Normal_StatusCol()%}"
