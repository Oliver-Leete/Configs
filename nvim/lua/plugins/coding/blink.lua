return {
    {
        "saghen/blink.compat",
        lazy = true,
        opts = {},
    },
    {
        "saghen/blink.cmp",
        build = "cargo build --release",
        dependencies = {
            { "kdheepak/cmp-latex-symbols" },
            { "rcarriga/cmp-dap" },
        },
        opts_extend = {
            "sources.completion.enabled_providers",
            "sources.default",
        },
        opts = {
            sources = {
                default = { "lsp", "path", "snippets", "buffer", "latex_symbols" },
                providers = {
                    latex_symbols = { name = "latex_symbols", module = "blink.compat.source" },
                    dap = { name = "dap", module = "blink.compat.source" },
                },
            },
            keymap = {
                ["<C-space>"] = { "show", "hide" },

                ["<Up>"] = { "select_prev", "fallback" },
                ["<Down>"] = { "select_next", "fallback" },
                ["<cr>"] = { "accept", "fallback" },

                ["<C-b>"] = { "scroll_documentation_up", "fallback" },
                ["<C-f>"] = { "scroll_documentation_down", "fallback" },

                ["<Tab>"] = { "snippet_forward", "fallback" },
                ["<S-Tab>"] = { "snippet_backward", "fallback" },

                ["<C-k>"] = { "show_signature", "hide_signature", "fallback" },
            },
            completion = {
                keyword = { range = "prefix", },
                accept = { auto_brackets = { enabled = true, }, },
                list = { selection = { preselect = false, auto_insert = true } },
                menu = { auto_show = true, },
                documentation = { auto_show = true, auto_show_delay_ms = 125 },
            },
            signature = { enabled = true },
            cmdline = {
                keymap = {
                    preset = "inherit",
                    ["<cr>"] = {},
                },
                enabled = true,
                completion = {
                    menu = { auto_show = true },
                }
            },
        },
        ---@param opts blink.cmp.Config | { sources: { compat: string[] } }
        config = function(_, opts)
            local is_dap_buffer = function()
                return require("cmp_dap").is_dap_buffer()
            end

            local enabled = opts.sources.default
            if type(enabled) == "table" then
                local og_defaults = vim.deepcopy(enabled)

                local default_source_func = function(_)
                    if is_dap_buffer() then
                        return { "dap", "snippets", "buffer", "latex_symbols" }
                    else
                        return og_defaults
                    end
                end

                opts.sources.default = default_source_func
            end

            require("blink.cmp").setup(opts)
        end
    }
}
