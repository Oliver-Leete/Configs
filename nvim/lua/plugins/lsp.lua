local lsp_setup = function()
    require("mason-tool-installer").setup({
        ensure_installed = {
            "codelldb",
            "cpptools",
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

    local lspconfig = require("lspconfig")

    require("lspconfig.ui.windows").default_options.border = require("config.options").border

    vim.api.nvim_set_hl(0, "LspInlayHint", { link = "NvimDapVirtualText" })

    local custom_attach = function(client, bufnr)
        local sc = client.server_capabilities
        local bmap = function(mode, key, action, opts)
            vim.keymap.set(mode, key, action,
                vim.tbl_extend("force", { buffer = bufnr }, opts))
        end

        if client.name == "ruff" then
            sc.renameProvider = false
            sc.definitionProvider = false
            sc.referencesProvider = false
        end

        -- LSP Binding Override
        if client.name ~= "null-ls" then
            bmap("n", "gd", function() require("snacks.picker").lsp_definitions() end, { desc = "Deffinition" })
            bmap("n", "gr", function() require("snacks.picker").lsp_references() end, { desc = "References" })
            bmap("n", "gD", function() require("snacks.picker").lsp_type_definitions() end, { desc = "Type Deffinition" })
            bmap("n", "gI", function() require("snacks.picker").lsp_implementations() end, { desc = "Implementations" })

            bmap("n", "go", function() require("trouble").toggle("lsp_outgoing_calls") end, { desc = "Outgoing Calls" })
            bmap("n", "gi", function() require("trouble").toggle("lsp_incoming_calls") end, { desc = "Incoming Calls" })
        end
        bmap("n", ",rr", vim.lsp.buf.rename, { desc = "Rename variable" })
        bmap("n", "<C-,>", vim.lsp.codelens.run, { desc = "Run code lens" })
        bmap({ "n", "x" }, "<C-.>", vim.lsp.buf.code_action, { desc = "Run code actions" })
        if sc.codeLensProvider and sc.codeLensProvider == true then
            vim.lsp.codelens.refresh()
            vim.api.nvim_create_autocmd({ "BufEnter", "CursorHold", "InsertLeave" }, {
                callback = vim.lsp.codelens.refresh,
                buffer = bufnr,
                group = vim.api.nvim_create_augroup("lsp_autocmd", { clear = true }),
            })
        end
    end

    vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('UserLspConfig', {}),
        callback = function(ev)
            local bufnr = ev.buf
            local client = vim.lsp.get_client_by_id(ev.data.client_id)
            custom_attach(client, bufnr)
        end,
    })

    local capabilities = require("blink.cmp").get_lsp_capabilities()

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
    lspconfig.just.setup(default)
    lspconfig.fish_lsp.setup(default)

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




    lspconfig.lua_ls.setup({
        capabilities = capabilities,
        flags = { debounce_text_changes = 1000 },
        settings = {
            Lua = {
                workspace = {
                  checkThirdParty = false,
                },
                codeLens = {
                  enable = true,
                },
                completion = {
                  callSnippet = "Replace",
                },
                doc = {
                  privateName = { "^_" },
                },
                hint = {
                  enable = true,
                  setType = false,
                  paramType = true,
                  paramName = "Disable",
                  semicolon = "Disable",
                  arrayIndex = "Disable",
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
end

return {
    {
        "neovim/nvim-lspconfig",
        dependencies = {
            { "yioneko/nvim-type-fmt" },
            { "saghen/blink.cmp" },
            {
                "williamboman/mason.nvim",
                opts = {},
                dependencies = {
                    { "williamboman/mason-lspconfig.nvim", opts = {} },
                    "WhoIsSethDaniel/mason-tool-installer.nvim",
                }
            },
            { "barreiroleo/ltex_extra.nvim" },

        },
        config = lsp_setup
    },
}
