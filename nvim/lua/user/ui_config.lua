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
local tc = require("kanagawa.colors").setup()

local background = vim.api.nvim_get_hl_by_name("CursorLine", true).background

local sign_colours = { Add = "Added", Change = "Changed", Delete = "Deleted" }
for sign, colour in pairs(sign_colours) do
    local highlight = vim.api.nvim_get_hl_by_name("diff" .. colour, true)
    highlight.background = background
    vim.api.nvim_set_hl(0, "GitSigns" .. sign .. "Cul", highlight)
end

vim.api.nvim_set_hl(0, "CursorLineNr", { fg = tc.roninYellow, bg = tc.sumiInk3, bold = true })
vim.api.nvim_set_hl(0, "CursorLineSign", { link = "CursorLine" })
vim.api.nvim_set_hl(0, "CursorLineFold", { link = "CursorLine" })

vim.g.matchup_matchparen_deferred = true
vim.g.matchup_matchparen_hi_surround_always = true

vim.api.nvim_set_hl(0, "IndentBlanklineContextChar", { link = "MatchParen" })
vim.api.nvim_set_hl(0, "IndentBlanklineContextStart", { underline = true, sp = "#ff9e3b" })

vim.api.nvim_set_hl(0, "MiniMapNormal", { fg = tc.fujiWhite })

require("indent_blankline").setup {
    char = "",
    context_char = "▎",
    space_char_blankline = " ",
    show_current_context = true,
    show_current_context_start = true,
    show_current_context_start_on_current_line = true,
    context_patterns = {
        "class",
        "^func",
        "method",
        "^if",
        "while",
        "for",
        "with",
        "try",
        "except",
        "arguments",
        "argument_list",
        "object",
        "dictionary",
        "element",
        "table",
        "tuple",
        "do_block",
        "array",
        "struct",
    },
}

require("notify").setup({
    top_down = false,
})

vim.notify = require("notify")

local mode_colours = { Normal = tc.crystalBlue, Insert = tc.autumnGreen, Visual = tc.oniViolet, Replace = tc.autumnRed,
    Command = tc.boatYellow2, Inactive = tc.fujiGray }
for mode, colour in pairs(mode_colours) do
    vim.api.nvim_set_hl(0, "WinBar" .. mode, { fg = tc.bg, bg = colour })
    vim.api.nvim_set_hl(0, "WinBar" .. mode .. "Ends", { fg = colour, bg = tc.bg })
    vim.api.nvim_set_hl(0, "WinBar" .. mode .. "MidEnds", { fg = colour, bg = tc.fujiGray })

end
vim.api.nvim_set_hl(0, "WinBarInactiveSpecial", { fg = tc.bg, bg = tc.waveBlue2 })
vim.api.nvim_set_hl(0, "WinBarInactiveSpecialEnds", { fg = tc.waveBlue2, bg = tc.bg })
vim.api.nvim_set_hl(0, "WinBarBlank", { fg = tc.sumiInk, bg = tc.sumiInk })
vim.api.nvim_set_hl(0, "WinBarBlank", { fg = tc.sumiInk, bg = tc.sumiInk })

vim.api.nvim_set_hl(0, "TabLine", { fg = tc.bg, bg = tc.fujiGray, sp = tc.sumiInk3 })
vim.api.nvim_set_hl(0, "TabLineEnds", { fg = tc.fujiGray, bg = tc.bg, sp = tc.sumiInk3 })
vim.api.nvim_set_hl(0, "TabLineActive", { fg = tc.bg, bg = tc.crystalBlue, sp = tc.sumiInk3 })
vim.api.nvim_set_hl(0, "TabLineActiveEnds", { fg = tc.crystalBlue, bg = tc.bg, sp = tc.sumiInk3 })

require("nvim-navic").setup({
    highlight = false,
    separator = "  "
})

require("noice").setup({
    popupmenu = {
        enabled = true,
        backend = "cmp",
    },
    command = {
        history = {
            view = "split",
        },
    },
    messages = {
        view_history = "split",
        view = false,
        view_error = "mini",
        view_warn = false,
    },
    routes = {
        {
            view = "split",
            filter = { event = "msg_show", min_width = 100, min_height = 20 },
        },
    },
    lsp = {
        progress = {
            enabled = true,
            view = "mini",
        },
        message = {
            enabled = true,
            view = "mini",
        },
        documentation = {
            view = "hover",
            opts = {
                lang = "markdown",
                replace = true,
                render = "plain",
                format = { "{message}" },
                position = { row = 2 },
                border = { style = Border },
                win_options = { winblend=0, concealcursor = "n", conceallevel = 3 },
            },
        },
        override = {
            ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
            ["vim.lsp.util.stylize_markdown"] = true,
            ["cmp.entry.get_documentation"] = true,
        },
    },
    presets = {
        command_palette = false,
    },
    views = {
        split = {
            enter = true,
            position = "top",
            win_options = {
                winhighlight = {
                    Normal = "Normal",
                },
            }
        }
    },
})
