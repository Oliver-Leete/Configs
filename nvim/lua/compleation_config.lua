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

local has_words_before = function()
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
end
local t = function(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end
local cmp = require("cmp")
local luasnip = require("luasnip")
require("cmp").setup({
    snippet = {
        expand = function(args)
            require("luasnip").lsp_expand(args.body)
        end,
    },
    mapping = {
        ["<C-b>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
        ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
        ["<down>"] = cmp.mapping({
            c = function()
                if cmp.visible() then
                    cmp.select_next_item({ behavior = cmp.SelectBehavior.Insert })
                else
                    vim.api.nvim_feedkeys(t("<Down>"), "n", true)
                end
            end,
            i = function()
                if cmp.visible() then
                    cmp.select_next_item({ behavior = cmp.SelectBehavior.Insert })
                elseif luasnip.expand_or_jumpable() then
                    luasnip.expand_or_jump()
                -- elseif has_words_before() then
                --     cmp.complete()
                else
                    vim.api.nvim_feedkeys(t("<tab>"), "n", true)
                end
            end,
            s = function()
                if cmp.visible() then
                    cmp.select_next_item({ behavior = cmp.SelectBehavior.Insert })
                elseif luasnip.expand_or_jumpable() then
                    luasnip.expand_or_jump()
                else
                    vim.api.nvim_feedkeys(t("<tab>"), "n", true)
                end
            end,
        }),
        ["<up>"] = cmp.mapping({
            c = function()
                if cmp.visible() then
                    cmp.select_prev_item({ behavior = cmp.SelectBehavior.Insert })
                else
                    vim.api.nvim_feedkeys(t("<up>"), "n", true)
                end
            end,
            i = function()
                if cmp.visible() then
                    cmp.select_prev_item({ behavior = cmp.SelectBehavior.Insert })
                elseif luasnip.jumpable(-1) then
                    luasnip.jump(-1)
                else
                    vim.api.nvim_feedkeys(t("<s-tab>"), "n", true)
                end
            end,
            s = function()
                if cmp.visible() then
                    cmp.select_prev_item({ behavior = cmp.SelectBehavior.Insert })
                elseif luasnip.jumpable(-1) then
                    luasnip.jump(-1)
                else
                    vim.api.nvim_feedkeys(t("<s-tab>"), "n", true)
                end
            end
        }),
        ["<CR>"] = cmp.mapping(cmp.mapping.confirm({ select = false }), { "i" }),
    },

    sources = {
        { name = "luasnip" },
        { name = "cmp_git" },
        { name = "nvim_lsp" },
        { name = "path" },
        { name = "nvim_lua" },
        { name = "buffer", keyword_lenght = 3 },
        { name = "dictionary", keyword_lenght = 2},
    },
    formatting = {
        format = function(entry, vim_item)
            vim_item.menu = ({
                luasnip = "(LSnip)",
                nvim_lsp = "(LSP)",
                path = "(Path)",
                nvim_lua = "(Lua)",
                buffer = "(Buffer)",
                cmdline = "(CMD)",
                dictionary = "(DICT)"
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

require("cmp_dictionary").setup({
    dic = {
        ["tex"] = "/home/oleete/.config/nvim/pluged/cmp-dictionary/british_english.dic",
        ["markdown"] = "/home/oleete/.config/nvim/pluged/cmp-dictionary/british_english.dic",
    },
})

local cmp_autopairs = require("nvim-autopairs.completion.cmp")
cmp.event:on("confirm_done", cmp_autopairs.on_confirm_done({ map_char = { all = "(", tex = "{", haskell = " " } }))

-- AutoPairs Setup
require("nvim-autopairs").setup({
    check_ts = true,
    enable_check_bracket_line = true,
    fast_wrap = {
        map = "<C-p>",
        chars = { "{", "[", "(", '"', "'", "`" },
        pattern = string.gsub([[ [%'%"%)%>%]%)%}%,%s] ]], "%s+", ""),
        end_key = "l",
        keys = "tnseriaodhgjplfuwybkvmcxzq",
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

require('tabout').setup {
    tabouts = {
      {open = "'", close = "'"},
      {open = '"', close = '"'},
      {open = '`', close = '`'},
      {open = '(', close = ')'},
      {open = '[', close = ']'},
      {open = '{', close = '}'},
      {open = '[[', close = ']]'},
      {open = '\\[', close = '\\]'},
      {open = '\\(', close = '\\)'},
      {open = '```', close = '```'},
      {open = '"""', close = '"""'},
    },
}

local function replace_keycodes(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

_G.cmp_toggle = function()
    if require("cmp").visible() then
        return replace_keycodes([[<cmd>lua require("cmp").close()<cr>]])
    else
        return replace_keycodes([[<cmd>lua require("cmp").complete()<cr>]])
    end
end

_G.cmp_esc = function()
    if require("cmp").visible() then
        return replace_keycodes([[<cmd>lua require("cmp").close()<cr>]])
    else
        return replace_keycodes("<esc>")
    end
end
