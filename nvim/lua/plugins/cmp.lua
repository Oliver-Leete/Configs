local cmp_setup = function(_, opts)
    vim.o.completeopt = "menuone,noselect"
    local cmp = require("cmp")

    local buffer = {
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
    }
    cmp.setup(opts)


    cmp.setup.cmdline("/", {
        sources = {
            buffer,
            { name = "cmdline_history" },
            { name = "latex_symbols" },
        },
    })

    cmp.setup.cmdline(":", {
        sources = cmp.config.sources({
            { name = "path" },
            buffer,
            { name = "cmdline_history" },
            { name = "cmdline" },
            { name = "latex_symbols" },
        }),
    })
    cmp.setup.filetype("tex", {
        sources = {
            { name = "vimtex" },
            { name = "nvim_lsp" },
            { name = "path",    option = { get_cwd = function() return vim.fn.getcwd() end, } },
            buffer,
        },
    })

    cmp.setup.filetype({ "dap-repl", "dapui_watches", "dapui_hover" }, { sources = { { name = "dap" }, } })
end
return {
    "hrsh7th/nvim-cmp",
    dependencies = {
        { "onsails/lspkind.nvim" },
        { "hrsh7th/cmp-buffer" },
        { "hrsh7th/cmp-path" },
        { "hrsh7th/cmp-nvim-lsp" },
        { "hrsh7th/cmp-cmdline" },
        { "dmitmel/cmp-cmdline-history" },
        { "micangl/cmp-vimtex" },
        { "kdheepak/cmp-latex-symbols" },
        { "rcarriga/cmp-dap" },
    },
    opts = function()
        local buffer = {
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
        }

        local cmp = require("cmp")
        return {
            mapping = {
                ["<C-b>"] = cmp.mapping(cmp.mapping.scroll_docs(-4), { "i", "c" }),
                ["<C-f>"] = cmp.mapping(cmp.mapping.scroll_docs(4), { "i", "c" }),
                ["<down>"] = cmp.mapping(cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
                    { "i", "s", "c" }),
                ["<up>"] = cmp.mapping(cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
                    { "i", "s", "c" }),
                ["<CR>"] = cmp.mapping(cmp.mapping.confirm({ select = false }), { "i" }),
                ["<C-CR>"] = cmp.mapping(cmp.mapping.confirm({ select = false }), { "c" }),
            },
            sources = {
                { name = "nvim_lsp" },
                { name = "latex_symbols" },
                { name = "path" },
                buffer,
            },
            formatting = { fields = { "abbr", "kind", "menu" }, format = require("lspkind").cmp_format({ mode = "symbol", maxwidth = 50 }), },
            enabled = function()
                return vim.api.nvim_get_option_value("buftype", { buf = 0 }) ~= "prompt" or
                    require("cmp_dap").is_dap_buffer()
            end,
            sorting = { comparators = { function(...) return require('cmp_buffer'):compare_locality(...) end, },
            },
        }
    end,
    config = cmp_setup,
    version = false,
    event = "InsertEnter",
}
