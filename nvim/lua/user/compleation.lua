vim.o.completeopt = "menuone,noselect"

local M = {}
M.icons = {
    Text = '  ',
    Method = '  ',
    Function = '  ',
    Constructor = '  ',
    Field = '  ',
    Variable = '  ',
    Class = '  ',
    Interface = '  ',
    Module = '  ',
    Property = '  ',
    Unit = '  ',
    Value = '  ',
    Enum = '  ',
    Keyword = '  ',
    Snippet = '  ',
    Color = '  ',
    File = '  ',
    Reference = '  ',
    Folder = '  ',
    EnumMember = '  ',
    Constant = '  ',
    Struct = '  ',
    Event = '  ',
    Operator = '  ',
    TypeParameter = '  ',
}

M.sources = {
    luasnip_choice = "(CHOICE)",
    luasnip = "(SNIP)",
    git = "(GIT)",
    otter = "(LSP)",
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
}

Map("c", "<C-CR>", "<nop>")

local cmp = require("cmp")
require("cmp").setup({
    snippet = {
        expand = function(args)
            require("luasnip").lsp_expand(args.body)
        end,
    },
    window = {
        completion = cmp.config.window.bordered({
              winhighlight = "Normal:Pmenu,FloatBorder:Pmenu,Search:None",
              col_offset = -3,
              side_padding = 0,
        }),
        documentation = cmp.config.window.bordered(),

    },
    mapping = {
        ["<C-k>"] = cmp.mapping(cmp.mapping.scroll_docs(-5), { "i", "c" }),
        ["<C-j>"] = cmp.mapping(cmp.mapping.scroll_docs(5), { "i", "c" }),
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
        ["<C-CR>"] = cmp.mapping(cmp.mapping.confirm({ select = false }), { "c" }),
    },
    sources = {
        { name = "luasnip_choice" },
        { name = "luasnip" },
        { name = "cmp_git" },
        { name = 'otter' },
        { name = "nvim_lsp" },
        { name = "latex_symbols" },
        { name = "fish" },
        { name = "path" },
        { name = "nvim_lua" },
        {
            name = "buffer",
            keyword_length = 3,
            option = {
                get_bufnrs = function()
                    local bufs = {}
                    for _, win in ipairs(vim.api.nvim_list_wins()) do
                        bufs[vim.api.nvim_win_get_buf(win)] = true
                    end
                    return vim.tbl_keys(bufs)
                end
            },
        },
    },
    formatting = {
        fields = { "kind", "abbr", "menu" },
        format = function(entry, vim_item)
            vim_item.menu = M.sources[entry.source.name]
            vim_item.kind = M.icons[vim_item.kind]
            return vim_item
        end,
    },
    enabled = function()
        return vim.api.nvim_buf_get_option(0, "buftype") ~= "prompt"
            or require("cmp_dap").is_dap_buffer()
    end,
    sorting = {
        comparators = {
            function(...) return require('cmp_buffer'):compare_locality(...) end,
        },
    },
})

require('cmp_luasnip_choice').setup({
    auto_open = true,
})

cmp.setup.cmdline("/", {
    sources = {
        { name = 'nvim_lsp_document_symbol' },
        {
            name = "buffer",
            keyword_length = 3,
            option = {
                get_bufnrs = function()
                    local bufs = {}
                    for _, win in ipairs(vim.api.nvim_list_wins()) do
                        bufs[vim.api.nvim_win_get_buf(win)] = true
                    end
                    return vim.tbl_keys(bufs)
                end
            },
        },
        { name = "cmdline_history" },
        { name = "latex_symbols" },
    },
})

cmp.setup.cmdline(":", {
    sources = cmp.config.sources({
        { name = "path" },
        {
            name = "buffer",
            keyword_length = 3,
            option = {
                get_bufnrs = function()
                    local bufs = {}
                    for _, win in ipairs(vim.api.nvim_list_wins()) do
                        bufs[vim.api.nvim_win_get_buf(win)] = true
                    end
                    return vim.tbl_keys(bufs)
                end
            },
        },
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
        {
            name = "path",
            option = {
                get_cwd = function()
                    return vim.fn.getcwd()
                end,
            }
        },
        {
            name = "buffer",
            keyword_length = 3,
            option = {
                get_bufnrs = function()
                    local bufs = {}
                    for _, win in ipairs(vim.api.nvim_list_wins()) do
                        bufs[vim.api.nvim_win_get_buf(win)] = true
                    end
                    return vim.tbl_keys(bufs)
                end
            },
        },
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


require("cmp").setup.filetype("DressingInput", {
    sources = cmp.config.sources { { name = "omni" } },
})

local crates_augroup = vim.api.nvim_create_augroup("crates", { clear = true })
vim.api.nvim_create_autocmd(
    "Filetype",
    {
        pattern = "toml",
        callback = function()
            require('cmp').setup.buffer({ sources = { { name = 'crates' } } })
        end,
        group = crates_augroup
    }
)

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
    languages = {
        python = {
            template = {
                annotation_convention = "numpydoc"
            }
        }
    }
})

local npairs = require('nvim-autopairs')

npairs.setup({
    disable_in_macro = true,
    disable_in_visualblock = true,
    check_ts = true,
    enable_afterquote = false
})

local cond = require('nvim-autopairs.conds')
local rule = require('nvim-autopairs.rule')
local endwise = require('nvim-autopairs.ts-rule').endwise

npairs.add_rules({
    rule("$", "$", { "tex", "latex" }):with_move(cond.none()),
    rule("[", "]", { "lua" }):with_move(cond.none()),
})

npairs.add_rules({
    endwise('function.*(.*)$', 'end', 'julia', 'function_definition'),
    endwise('for .*$', 'end', 'julia', 'for_statement'),
    endwise('while .*$', 'end', 'julia', 'while_statement'),
    endwise('if .*$', 'end', 'julia', 'if_statement'),
    endwise('let.*$', 'end', 'julia', 'let_statement'),
    endwise('begin$', 'end', 'julia', 'compound_statement'),
    endwise('try$', 'end', 'julia', 'try_statement'),
    endwise('(.*) do.*$', 'end', 'julia', nil),
    endwise('module$', 'end', 'julia', nil),
})

Ls = require("luasnip")
Ls.config.set_config({
    history = false,
    update_events = "TextChanged,TextChangedI",
    delete_check_events = "TextChanged",
})
require("luasnip.loaders.from_lua").load({ paths = "/home/oleete/.config/nvim/snippets" })
require("luasnip-latex-snippets").setup()

require('tabout').setup {
    tabkey = '',
    backwards_tabkey = '',
    act_as_tab = true,
    act_as_shift_tab = true,
    enable_backwards = true,
    completion = false,
    tabouts = {
        { open = "'",   close = "'" },
        { open = '"',   close = '"' },
        { open = '`',   close = '`' },
        { open = '(',   close = ')' },
        { open = '[',   close = ']' },
        { open = '{',   close = '}' },
        { open = '[[',  close = ']]' },
        { open = '```', close = '```' },
        { open = '"""', close = '"""' },
        { open = '$',   close = '$' },
    },
    ignore_beginning = true,
    exclude = {}
}
