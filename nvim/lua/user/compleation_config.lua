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
        ["<down>"] = cmp.mapping({
            i = function()
                if cmp.visible() then
                    cmp.select_next_item({ behavior = cmp.SelectBehavior.Insert })
                end
            end,
            s = function()
                if cmp.visible() then
                    cmp.select_next_item({ behavior = cmp.SelectBehavior.Insert })
                end
            end,
            c = function()
                cmp.select_next_item({ behavior = cmp.SelectBehavior.Insert })
            end,
        }),
        ["<up>"] = cmp.mapping({
            i = function()
                if cmp.visible() then
                    cmp.select_prev_item({ behavior = cmp.SelectBehavior.Insert })
                end
            end,
            s = function()
                if cmp.visible() then
                    cmp.select_prev_item({ behavior = cmp.SelectBehavior.Insert })
                end
            end,
            c = function()
                cmp.select_prev_item({ behavior = cmp.SelectBehavior.Insert })
            end,
        }),
        ["<CR>"] = cmp.mapping(cmp.mapping.confirm({ select = false }), { "i" }),
    },

    sources = {
        { name = "luasnip" },
        { name = "cmp_git" },
        { name = "nvim_lsp" },
        { name = "latex_symbols" },
        { name = "fish" },
        { name = "path" },
        { name = "nvim_lua" },
        { name = "buffer", keyword_length = 3 },
    },
    formatting = {
        format = function(entry, vim_item)
            vim_item.menu = ({
                luasnip = "(SNIP)",
                git = "(GIT)",
                nvim_lsp = "(LSP)",
                latex_symbols = "(SYM)",
                fish = "(FISH)",
                path = "(PATH)",
                nvim_lua = "(NVIM)",
                buffer = "(BUFF)",
                cmdline = "(CMD)",
                cmdline_history = "(CMDH)",
                dictionary = "(DICT)",
                omni = "(TEX?)",
                nvim_lsp_document_symbol = "(LSP)",
            })[entry.source.name]
            vim_item.kind = M.icons[vim_item.kind]
            return vim_item
        end,
    },
    enabled = function()
        return vim.api.nvim_buf_get_option(0, "buftype") ~= "prompt"
            or require("cmp_dap").is_dap_buffer()
    end
})

cmp.setup.cmdline("/", {
    sources = {
        { name = 'nvim_lsp_document_symbol' },
        { name = "buffer" },
        { name = "cmdline_history" },
        { name = "latex_symbols" },
    },
})

cmp.setup.cmdline(":", {
    sources = cmp.config.sources({
        { name = "path" },
        { name = "buffer" },
        { name = "cmdline_history" },
        { name = "cmdline" },
        { name = "latex_symbols" },
    }),
})
require("cmp").setup.filetype("tex", {
    sources = {
        { name = "luasnip" },
        { name = "omni" },
        { name = "nvim_lsp" },
        { name = "path" },
        { name = "buffer", keyword_length = 3 },
        { name = "dictionary", keyword_length = 3 },
    },
})

require("cmp_git").setup({})

require("cmp_dictionary").setup({
    dic = {
        ["tex"] = "/home/oleete/.config/nvim/pluged/cmp-dictionary/british_english.dic",
        ["markdown"] = "/home/oleete/.config/nvim/pluged/cmp-dictionary/british_english.dic",
    },
})

require("cmp").setup.filetype({ "dap-repl", "dapui_watches", "dapui_hover" }, {
    sources = {
        { name = "dap" },
    },
})

vim.cmd([[autocmd FileType toml lua require('cmp').setup.buffer { sources = { { name = 'crates' } } }]])

vim.g.diagnostic_auto_popup_while_jump = 0
vim.g.diagnostic_enable_virtual_text = 0
vim.g.diagnostic_enable_underline = 0
vim.g.completion_timer_cycle = 200

_G.cmp_toggle = function()
    if require("cmp").visible() then
        require("cmp").close()
    else
        require("cmp").complete()
    end
end

-- Neogen setup
require("neogen").setup({
    snippet_engine = "luasnip",
})
