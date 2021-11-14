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

-- Compleation (cmp) and related pairs stuff

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
        ["<C-b>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
        ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
        ["<Down>"] = cmp.mapping(cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }), { "c" }),
        ["<Up>"] = cmp.mapping(cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }), { "c" }),
        ["<tab>"] = cmp.mapping(cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }), { "c" }),
        ["<s-tab>"] = cmp.mapping(cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }), { "c" }),
        ["<CR>"] = cmp.mapping(cmp.mapping.confirm({ select = false }), { "i"}),
    },
    sources = {
        { name = "luasnip" },
        { name = "cmp_git" },
        { name = "cmp_tabnine", keyword_length = 3 },
        { name = "nvim_lsp" },
        { name = "path" },
        { name = "nvim_lua" },
        { name = "buffer", keyword_lenght = 3 },
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
                cmdline = "(CMD)",
            })[entry.source.name]
            vim_item.kind = M.icons[vim_item.kind]
            return vim_item
        end,
    },
})

cmp.setup.cmdline("/", {
    sources = {
        { name = "buffer" },
    },
})

cmp.setup.cmdline(":", {
    sources = cmp.config.sources({
        { name = "path" },
        { name = "buffer" },
        { name = "cmdline" },
    }),
})

require("cmp_git").setup({})

local cmp_autopairs = require("nvim-autopairs.completion.cmp")
cmp.event:on( 'confirm_done', cmp_autopairs.on_confirm_done({  map_char = { all = "(", tex = '{', haskell = " "} }))

-- AutoPairs Setup
require("nvim-autopairs").setup({
    check_ts = true,
    enable_check_bracket_line = true,
    fast_wrap = {
        map = "<C-p>",
        chars = { "{", "[", "(", '"', "'", "`" },
        pattern = string.gsub([[ [%'%"%)%>%]%)%}%,%s] ]], '%s+', ''),
        end_key = "l",
        keys   = "tnseriaodhgjplfuwybkvmcxzq",
    },
})

-- require('nvim-autopairs').remove_rule('"')

local Rule = require("nvim-autopairs.rule")
local cond = require("nvim-autopairs.conds")
require("nvim-autopairs").add_rules(
    { Rule("$", "$", { "tex", "markdown" }):with_move(cond.none()) },
    { Rule("$$", "$$", { "tex", "markdown" }):with_move(cond.none()) }
)

require("nvim-autopairs").add_rules({
    Rule("\\(", "\\)", "tex"),
    Rule("\\[", "\\]", "tex"),
    Rule("#=", "=#", "julia"),
    Rule("```", "```"),
})

vim.g.diagnostic_auto_popup_while_jump = 0
vim.g.diagnostic_enable_virtual_text = 0
vim.g.diagnostic_enable_underline = 0
vim.g.completion_timer_cycle = 200
