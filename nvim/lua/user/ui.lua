-- Theme
vim.opt.termguicolors = true
vim.opt.guifont = "JuliaMono:h10"

require("stickybuf").setup({
    filetype = {
        OverseerList = "bufnr",
    },
    buftype = {
        terminal = "buftype",
        help = "buftype",
    },
})

require("dressing").setup({
    select = {
        backend = { "telescope" },
        telescope = require("telescope.themes").get_ivy({
            height = 30,
        }),
    },
})

require("kanagawa").setup({
    keywordStyle = {},
    statementStyle = {},
    variablebuiltinStyle = {},
    globalStatus = true,
})

vim.cmd("colorscheme kanagawa")
Tc = require("kanagawa.colors").setup()

local background = vim.api.nvim_get_hl_by_name("CursorLine", true).background

local sign_colours = { Add = "Added", Change = "Changed", Delete = "Deleted" }
for sign, colour in pairs(sign_colours) do
    local highlight = vim.api.nvim_get_hl_by_name("diff" .. colour, true)
    highlight.background = background
    vim.api.nvim_set_hl(0, "GitSigns" .. sign .. "Cul", highlight)
end

vim.api.nvim_set_hl(0, "CursorLineNr", { fg = Tc.sumiInk4, bg = Tc.sumiInk3, bold = true })
vim.api.nvim_set_hl(0, "CursorLineSign", { link = "CursorLine" })
vim.api.nvim_set_hl(0, "CursorLineFold", { link = "CursorLine" })

vim.g.matchup_matchparen_deferred = true
vim.g.matchup_matchparen_hi_surround_always = true

local CursorMatchParen = vim.api.nvim_get_hl_by_name("MatchParen", true)
CursorMatchParen.background = background
vim.api.nvim_set_hl(0, "CursorMatchParen", CursorMatchParen)

require("notify").setup({
    top_down = false,
})

vim.notify = require("notify")

local mode_colours = { Normal = Tc.crystalBlue, Insert = Tc.autumnGreen, Visual = Tc.oniViolet, Replace = Tc.autumnRed,
    Command = Tc.boatYellow2, Inactive = Tc.fujiGray }
for mode, colour in pairs(mode_colours) do
    vim.api.nvim_set_hl(0, "WinBar" .. mode, { fg = Tc.bg, bg = colour })
    vim.api.nvim_set_hl(0, "WinBar" .. mode .. "Ends", { fg = colour, bg = Tc.bg })
    vim.api.nvim_set_hl(0, "WinBar" .. mode .. "MidEnds", { fg = colour, bg = Tc.fujiGray })
    vim.api.nvim_set_hl(0, "WinBar" .. mode .. "NavicEnds", { fg = colour, bg = Tc.winterBlue })

end
vim.api.nvim_set_hl(0, "WinBarInactiveSpecial", { fg = Tc.bg, bg = Tc.waveBlue2 })
vim.api.nvim_set_hl(0, "WinBarInactiveSpecialEnds", { fg = Tc.waveBlue2, bg = Tc.bg })
vim.api.nvim_set_hl(0, "WinBarBlank", { fg = Tc.sumiInk, bg = Tc.sumiInk })
vim.api.nvim_set_hl(0, "WinBarBlank", { fg = Tc.sumiInk, bg = Tc.sumiInk })

vim.api.nvim_set_hl(0, "TabLine", { fg = Tc.bg, bg = Tc.fujiGray, sp = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "TabLineEnds", { fg = Tc.fujiGray, bg = Tc.bg, sp = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "TabLineActive", { fg = Tc.bg, bg = Tc.crystalBlue, sp = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "TabLineActiveEnds", { fg = Tc.crystalBlue, bg = Tc.bg, sp = Tc.sumiInk3 })

local colorful_winsep = require("colorful-winsep")
colorful_winsep.setup({
    -- Window divider color definition
    highlight = {
        bg = Tc.bg,
        fg = Tc.crystalBlue
    },
    -- timer refresh rate
    interval = 30,
    -- filetype in the list, will not be executed
    no_exec_files = { "packer", "TelescopePrompt", "mason", "CompetiTest", "NvimTree" },
    -- Split line symbol definition
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
            ['<C-u>'] = actions.preview_scroll_win(5),
            ['<C-d>'] = actions.preview_scroll_win(-5),
            ['<c-v>'] = actions.jump_vsplit,
            ['<c-x>'] = actions.jump_split,
            ['<CR>'] = actions.jump,
            ['o'] = actions.enter_win('preview'),
            ['<Esc>'] = actions.close,
        },
        preview = {
            ['n'] = actions.next_location,
            ['N'] = actions.previous_location,
            ['<esc>'] = actions.enter_win('list'),
            ['<leader><cr>'] = actions.enter_win('list'),
        },
    },
})
