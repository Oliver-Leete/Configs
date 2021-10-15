----------------------------------------------------------------------------------------------------
--                      _   _   ______    ____   __      __  _____   __  __                       --
--                     | \ | | |  ____|  / __ \  \ \    / / |_   _| |  \/  |                      --
--                     |  \| | | |__    | |  | |  \ \  / /    | |   | \  / |                      --
--                     | . ` | |  __|   | |  | |   \ \/ /     | |   | |\/| |                      --
--                     | |\  | | |____  | |__| |    \  /     _| |_  | |  | |                      --
--                     |_| \_| |______|  \____/      \/     |_____| |_|  |_|                      --
--                                                                                                --
----------------------------------------------------------------------------------------------------
-- Oliver Leete <oliverleete@gmail.com>                                                            --
-- https://github.com/oliver-leete                                                                 --
----------------------------------------------------------------------------------------------------

-- Compleation (cmp) and Snippet (luasnip) Setup

vim.o.completeopt = "menuone,noselect"

local M = {}
M.icons = {
    Class = "ﴯ",
    Color = "",
    Constant = "",
    Constructor = "",
    Enum = "",
    EnumMember = "",
    Event = "",
    Field = "ﰠ",
    File = "",
    Folder = "",
    Function = "",
    Interface = "",
    Keyword = "",
    Method = "",
    Module = "",
    Operator = "",
    Property = "ﰠ",
    Reference = "",
    Snippet = "",
    Struct = "",
    Text = "",
    TypeParameter = "",
    Unit = "",
    Value = "",
    Variable = "",
}

local cmp = require("cmp")
require("cmp").setup({
    snippet = {
        expand = function(args)
            require("luasnip").lsp_expand(args.body)
        end,
    },
    mapping = {
        ["<C-b>"] = cmp.mapping.scroll_docs(-4),
        ["<C-f>"] = cmp.mapping.scroll_docs(4),
        ['<Down>'] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
        ['<Up>'] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
        ["<CR>"] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Replace,
            select = true,
        }),
    },
    sources = {
        { name = "luasnip" },
        { name = "cmp_tabnine" },
        { name = "nvim_lsp" },
        { name = "path" },
        { name = "nvim_lua" },
        { name = "buffer" },
    },
    formatting = {
		format = function(entry, vim_item)
			vim_item.menu = ({
                luasnip = "(LSnip)",
                cmp_tabnine = "(Tab9)",
				nvim_lsp = "(LSP)",
                path = "(Path)",
                nvim_lua = "(Lua)",
				buffer = "(Buffer)",
			})[entry.source.name]
            vim_item.kind = M.icons[vim_item.kind]
			return vim_item
		end
    },
})

-- AutoPairs Setup
require("nvim-autopairs").setup({
    check_ts = true,
    enable_check_bracket_line = true,
    fast_wrap = {
        map = "<C-p>",
        chars = { "{", "[", "(", '"', "'", "`" },
        pattern = string.gsub([[ [%'%"%)%>%]%)%}%,] ]], "%s+", ""),
        end_key = "l",
        keys = "tnseriaodhgjplfuwybkvmcxzq",
    },
})

-- require('nvim-autopairs').remove_rule('"')

local Rule = require("nvim-autopairs.rule")
local cond = require('nvim-autopairs.conds')
require("nvim-autopairs").add_rules(
    {Rule("$", "$", {"tex", "markdown"})
        :with_move(cond.none())
    },
    {Rule("$$", "$$", {"tex", "markdown"})
        :with_move(cond.none())
    }
)

require("nvim-autopairs").add_rules({
    Rule("\\(", "\\)", "tex"),
    Rule("\\[", "\\]", "tex"),
    Rule("#=", "=#", 'julia'),
    Rule("```", "```"),
})

require("nvim-autopairs.completion.cmp").setup({
    map_cr = true,
    map_complete = true,
    insert = true,
    map_char = {
        all = '(',
        tex = '{',
        haskell = ' ',
    }
})

vim.g.diagnostic_auto_popup_while_jump = 0
vim.g.diagnostic_enable_virtual_text = 0
vim.g.diagnostic_enable_underline = 0
vim.g.completion_timer_cycle = 200

require("todo-comments").setup({
    signs = true,
    keywords = {
        FIX = { icon = " ", color = "error", alt = { "FIXME", "BUG", "FIXIT", "FIX", "ISSUE" } },
        TODO = { icon = " ", color = "info" },
        HACK = { icon = " ", color = "warning", alt = { "JANK", "WORKAROUND" } },
        WARN = { icon = " ", color = "warning", alt = { "WARNING" } },
        PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
        NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
    },
    highlight = {
        before = "",
        keyword = "bg",
        after = "fg",
    },
    colors = {
        error = { "LspDiagnosticsDefaultError", "ErrorMsg", "#DC2626" },
        warning = { "LspDiagnosticsDefaultWarning", "WarningMsg", "#FBBF24" },
        info = { "LspDiagnosticsDefaultInformation", "#2563EB" },
        hint = { "LspDiagnosticsDefaultHint", "#10B981" },
        default = { "Identifier", "#7C3AED" },
    },
})

require("tabout").setup({
    tabkey = "",
    backwards_tabkey = "",
    completion = false,
    enable_backwards = true,
    tabouts = {
        { open = "'", close = "'" },
        { open = '"', close = '"' },
        { open = "`", close = "`" },
        { open = "(", close = ")" },
        { open = "[", close = "]" },
        { open = "{", close = "}" },
        { open = "$", close = "$" },
        { open = "$$", close = "$$" },
        { open = "[[", close = "]]" },
        { open = "```", close = "```" },
        { open = '"""', close = '"""' },
        { open = '#=', close = '=#' },
        { open = '\\(', close = '\\)' },
        { open = '\\[', close = '\\]' },
        { open = "<", close = ">" },
    },
    ignore_beginning = true,
    exclude = {},
})
