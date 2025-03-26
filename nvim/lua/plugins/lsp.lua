local lsp_setup = function()
    vim.g.lsp_lens_on = true

    local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
    function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
        opts = opts or {}
        opts.border = opts.border or require("user.settings").border
        return orig_util_open_floating_preview(contents, syntax, opts, ...)
    end

    require("mason-tool-installer").setup({
        ensure_installed = {
            "codelldb",
            "cpptools",
            "debugpy",
            "esbonio",
            "fortls",
            "gitlint",
            "json-lsp",
            "julia-lsp",
            "lua-language-server",
            "markdownlint",
            "marksman",
            "rust-analyzer",
            "shellharden",
            "shfmt",
            "taplo",
            "texlab",
            "yaml-language-server",
        },
    })

    require("neodev").setup({})
    local lspconfig = require("lspconfig")

    require("lspconfig.ui.windows").default_options.border = require("user.settings").border

    vim.api.nvim_set_hl(0, "LspInlayHint", { link = "NvimDapVirtualText" })

    vim.diagnostic.config({
        underline = false,
        update_in_insert = false,
        severity_sort = true,
        virtual_lines = false,
        virtual_text = false,
        signs = {
            text = {
                [vim.diagnostic.severity.ERROR] = " ",
                [vim.diagnostic.severity.WARN] = " ",
                [vim.diagnostic.severity.INFO] = " ",
                [vim.diagnostic.severity.HINT] = "󰅽 ",
            },
        },
    })

    local lsp_auto = vim.api.nvim_create_augroup("lsp_autocmd", { clear = true })

    local custom_attach = function(client, bufnr)
        local sc = client.server_capabilities
        local bmap = function(mode, key, action, opts)
            Map(mode, key, action,
                vim.tbl_extend("force", { buffer = bufnr }, opts))
        end

        if client.name == "ruff" then
            sc.renameProvider = false
            sc.definitionProvider = false
            sc.referencesProvider = false
        end

        -- LSP Binding Override
        if client.name ~= "null-ls" then
            bmap("n", "gd", function() Snacks.picker.lsp_definitions() end, { desc = "Deffinition" })
            bmap("n", "gr", function() Snacks.picker.lsp_references() end, { desc = "References" })
            bmap("n", "gD", function() Snacks.picker.lsp_type_definitions() end, { desc = "Type Deffinition" })
            bmap("n", "gI", function() Snacks.picker.lsp_implementations() end, { desc = "Implementations" })

            bmap("n", "go", "<cmd>Telescope lsp_outgoing_calls jump_type=never theme=get_ivy<cr>",
                { desc = "Outgoing Calls" })
            bmap("n", "gi", "<cmd>Telescope lsp_incoming_calls jump_type=never theme=get_ivy<cr>",
                { desc = "Incoming Calls" })
        end
        bmap("n", "<C-,>", vim.lsp.codelens.run, { desc = "Run code lens" })
        if sc.codeLensProvider and sc.codeLensProvider == true then
            vim.lsp.codelens.refresh()
            vim.api.nvim_create_autocmd(
                "TextChanged",
                {
                    callback = function()
                        if vim.g.lsp_lens_on then
                            vim.lsp.codelens.refresh()
                        end
                    end,
                    buffer = bufnr,
                    group = lsp_auto,
                }
            )
        end
        bmap("n", "<leader>a", function()
            local cursor_pos = vim.api.nvim_win_get_cursor(0)
            local cursor_col = cursor_pos[2]
            local cursor_line = cursor_pos[1]
            local hints = vim.lsp.inlay_hint.get({ bufnr = 0 })
            local hint = vim.tbl_filter(function(h)
                return (
                    h.inlay_hint.position.line == cursor_line - 1 and
                    h.inlay_hint.position.character == cursor_col + 1
                )
            end, hints)[1]
            if hint then
                local text = (hint.inlay_hint.paddingLeft and " " or "") .. hint.inlay_hint.label[1].value
                vim.api.nvim_put({ text }, "c", true, true)
            end
        end, { desc = "Insert inlay hint" })
        bmap({ "n", "x" }, "<C-.>", vim.lsp.buf.code_action, { desc = "Run code actions" })
    end

    vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('UserLspConfig', {}),
        callback = function(ev)
            local bufnr = ev.buf
            local client = vim.lsp.get_client_by_id(ev.data.client_id)
            custom_attach(client, bufnr)
        end,
    })

    local capabilities = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())

    local default = {
        capabilities = capabilities,
        flags = { debounce_text_changes = 1000 },
    }

    lspconfig.clangd.setup(default)
    lspconfig.esbonio.setup(default)
    lspconfig.fortls.setup(default)
    lspconfig.jsonls.setup(default)
    lspconfig.julials.setup(default)
    lspconfig.marksman.setup(default)
    lspconfig.nushell.setup(default)
    lspconfig.ruff.setup(default)
    lspconfig.taplo.setup(default)
    lspconfig.yamlls.setup(default)
    lspconfig.contextive.setup(default)

    lspconfig.basedpyright.setup({
        settings = {
            python = {
                analysis = {
                    useLibraryCodeForTypes = true,
                    diagnosticSeverityOverrides = {
                        diagnosticMode = "workspace",
                    },
                    diagnosticMode = "workspace",
                    typeCheckingMode = "basic",
                },
            },
        },
        capabilities = capabilities,
        flags = { debounce_text_changes = 1000 },
    })

    vim.g.haskell_tools = {
        hls = {
            settings = {
                haskell = {
                    plugin = {
                        rename = {
                            config = {
                                crossModule = true
                            },
                        },
                        semanticTokens = {
                            globalOn = true
                        },
                    }
                }
            }
        }
    }



    lspconfig.lua_ls.setup({
        capabilities = capabilities,
        flags = { debounce_text_changes = 1000 },
        settings = {
            Lua = {
                hint = {
                    arrayIndex = "Disable",
                    enable = true,
                    paramName = "Disable",
                },
                completion = {
                    callSnippet = "Replace",
                },
                diagnostics = {
                    globals = { 'vim' },
                },
                workspace = {
                    library = vim.api.nvim_get_runtime_file('', true),
                    checkThirdParty = false,
                },
                telemetry = {
                    enable = false,
                },
            }
        }
    })

    lspconfig.texlab.setup({
        flags = { debounce_text_changes = 1000 },
        root_dir = lspconfig.util.root_pattern(".git"),
        settings = {
            texlab = {
                build = {
                    args = { "-pdf", "-interaction=nonstopmode", "-synctex=1", "%f" },
                    executable = "latexmk",
                    onSave = false,
                    forwardSearchAfter = true,
                },
                forwardSearch = {
                    executable = "zathura",
                    args = { "--synctex-forward", "%l:1:%f", "%p" },
                    onSave = false,
                },
                chktex = {
                    onEdit = true,
                    onOpenAndSave = true,
                },
                latexFormatter = "latexindent",
                latexindent = {
                    ["local"] = ".latexindent.yaml",
                    modifyLineBreaks = true,
                }
            },
        },
    })

    require("ltex_extra").setup({
        load_langs = { "en-GB" },
        init_check = true,
        path = ".ltex",
        log_level = "none",
        server_opts = {
            capabilities = capabilities,
            settings = {
                ltex = {
                    language = "en-GB",
                    diagnosticSeverity = { MORFOLOGIK_RULE_EN_GB = "hint", default = "info" },
                    additionalRules = {
                        enablePickyRules = false,
                        motherTongue = "en-GB",
                    },
                    disabledRules = { ["en-GB"] = { "OXFORD_SPELLING_Z_NOT_S" } },
                },
            },
        },
    })

    local none_ls = require("null-ls")
    -- None LS
    none_ls.setup({
        diagnostics_format = "[#{c}] #{m} (#{s})",
        sources = {
            none_ls.builtins.code_actions.gitrebase,
            none_ls.builtins.diagnostics.fish,
            none_ls.builtins.diagnostics.editorconfig_checker,
            none_ls.builtins.diagnostics.gitlint,
            none_ls.builtins.diagnostics.markdownlint.with({ extra_args = { "--disable", "MD013", "MD046", "MD009" } }),
            none_ls.builtins.formatting.bibclean,
            none_ls.builtins.formatting.fish_indent,
            none_ls.builtins.formatting.just,
            none_ls.builtins.formatting.shellharden,
            none_ls.builtins.formatting.shfmt,
        },
    })
end

return {
    "neovim/nvim-lspconfig",
    dependencies = {
        { "mrcjkb/rustaceanvim" },
        { "yioneko/nvim-type-fmt" },
        {
            "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
            opts = {},
        },
        { "folke/neodev.nvim" },
        { "MrcJkb/haskell-tools.nvim" },
        { "hrsh7th/cmp-nvim-lsp" },
        {
            "williamboman/mason.nvim",
            opts = {},
            dependencies = {
                { "williamboman/mason-lspconfig.nvim", opts = {} },
                "jayp0521/mason-nvim-dap.nvim",
                "WhoIsSethDaniel/mason-tool-installer.nvim",
            }
        },
        {
            "nvimtools/none-ls.nvim",
            dependencies = { "nvim-lua/plenary.nvim" },
        },
        { "barreiroleo/ltex_extra.nvim" },

    },
    config = lsp_setup
}
