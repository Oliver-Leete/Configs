-- Theme
vim.opt.termguicolors = true
vim.opt.guifont = "JuliaMono:h10"

require("stickybuf").setup({
    filetype = {
        OverseerList = "filetype",
        toggleterm = "filetype",
    },
    buftype = {
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
    undercurl = true,
    commentStyle = { italic = true },
    functionStyle = {},
    keywordStyle = {},
    statementStyle = {},
    typeStyle = {},
    variablebuiltinStyle = {},
    specialReturn = true,
    specialException = true,
    transparent = false,
    colors = {},
    overrides = {},
    dimInactive = false,
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

require("desktop-notify").override_vim_notify()

-- require("notify").setup({
--     top_down = false,
-- })
--
-- vim.notify = require("notify")

-- LSP integration
local severity = {
    "error",
    "warn",
    "info",
    "info", -- map both hint and info to info?
}
vim.lsp.handlers["window/showMessage"] = function(_, method, params, _)
    ---@diagnostic disable-next-line: redundant-parameter
    vim.notify(method.message, severity[params.type])
end

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


local mode_map = {
    ['n'] = { 'NORMAL', 'Normal' },
    ['no'] = { 'O-PENDING', 'Visual' },
    ['nov'] = { 'O-PENDING', 'Visual' },
    ['noV'] = { 'O-PENDING', 'Visual' },
    ['no'] = { 'O-PENDING', 'Visual' },
    ['nt'] = { 'T-NORMAL', 'Normal' },
    ['niI'] = { 'NORMAL', 'Normal' },
    ['niR'] = { 'NORMAL', 'Normal' },
    ['niV'] = { 'NORMAL', 'Normal' },
    ['v'] = { 'VISUAL', 'Visual' },
    ['V'] = { 'V-LINE', 'Visual' },
    [''] = { 'V-BLOCK', 'Visual' },
    ['s'] = { 'SELECT', 'Visual' },
    ['S'] = { 'S-LINE', 'Visual' },
    [''] = { 'S-BLOCK', 'Visual' },
    ['i'] = { 'INSERT', 'Insert' },
    ['ic'] = { 'INSERT', 'Insert' },
    ['ix'] = { 'INSERT', 'Insert' },
    ['R'] = { 'REPLACE', 'Replace' },
    ['Rc'] = { 'REPLACE', 'Replace' },
    ['Rv'] = { 'V-REPLACE', 'Normal' },
    ['Rx'] = { 'REPLACE', 'Normal' },
    ['Rvc'] = { 'V-REPLACE', 'Replace' },
    ['Rvx'] = { 'V-REPLACE', 'Replace' },
    ['c'] = { 'COMMAND', 'Command' },
    ['cv'] = { 'EX', 'Command' },
    ['ce'] = { 'EX', 'Command' },
    ['r'] = { 'REPLACE', 'Replace' },
    ['rm'] = { 'MORE', 'Normal' },
    ['r?'] = { 'CONFIRM', 'Normal' },
    ['!'] = { 'SHELL', 'Normal' },
    ['t'] = { 'TERMINAL', 'Command' },
}

VimMode = function()
    local mode_code = vim.api.nvim_get_mode().mode
    if mode_map[mode_code] == nil then
        return { mode_code, 'Normal' }
    end
    return mode_map[mode_code]
end

Get_unique_bufname = function(bufnr, max_length)
    max_length = max_length or 24
    local bufname = vim.api.nvim_buf_get_name(bufnr)
    local all_bufers = vim.tbl_filter(function(buffer)
        return buffer.listed == 1 and buffer.name ~= bufname
    end, vim.fn.getbufinfo())
    local all_name = vim.tbl_map(function(buffer)
        return string.reverse(buffer.name)
    end, all_bufers)
    local tmp_name = string.reverse(bufname)
    local position = 1
    if #all_name > 1 then
        for _, other_name in pairs(all_name) do
            for i = 1, #tmp_name do
                if tmp_name:sub(i, i) ~= other_name:sub(i, i) then
                    if i > position then
                        position = i
                    end
                    break
                end
            end
        end
    end
    while position <= #tmp_name do
        if tmp_name:sub(position, position) == '/' then
            position = position - 1
            break
        end
        position = position + 1
    end
    local name = string.reverse(string.sub(tmp_name, 1, position))
    if #name > max_length then
        return vim.fn.pathshorten(name)
    end
    return name
end
