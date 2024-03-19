M = {}

vim.g.lsp_lens_on = true

local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
    opts = opts or {}
    opts.border = opts.border or Border
    return orig_util_open_floating_preview(contents, syntax, opts, ...)
end

require("mason-tool-installer").setup({
    ensure_installed = {
        "arduino-language-server",
        "bash-debug-adapter",
        "bash-language-server",
        "codelldb",
        "cpptools",
        "debugpy",
        "esbonio",
        "fortls",
        "gitlint",
        "json-lsp",
        "julia-lsp",
        "ltex-ls",
        "lua-language-server",
        "markdownlint",
        "marksman",
        "rust-analyzer",
        "shellharden",
        "shfmt",
        "taplo",
        "teal-language-server",
        "texlab",
        "yaml-language-server",
    },
})

require("neodev").setup({})
local lspconfig = require("lspconfig")

require("lspconfig.ui.windows").default_options.border = Border

vim.api.nvim_set_hl(0, "LspInlayHint", { link = "NvimDapVirtualText" })

vim.diagnostic.config({
    underline = false,
    virtual_text = { severity = { min = "Warn", }, },
    update_in_insert = false,
    severity_sort = true,
    virtual_lines = false,
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
    local bmap = function(mode, key, action) Map(mode, key, action, { buffer = bufnr }) end

    if client.name == "ruff" then
        sc.renameProvider = false
        sc.definitionProvider = false
        sc.referencesProvider = false
    end

    if sc.documentSymbolProvider then
        require("nvim-navic").attach(client, bufnr)
    end

    -- LSP Binding Override
    if client.name ~= "null-ls" then
        bmap("n", "gd", "<cmd>Glance definitions<cr>", { desc = "Peek Definition" })
        bmap("n", "gr", "<cmd>Glance references<cr>", { desc = "Peek References" })
        bmap("n", "gD", "<cmd>Glance type_definitions<cr>", { desc = "Peek Type Deffinition" })
        bmap("n", "gI", "<cmd>Glance implementations<cr>", { desc = "Peek implementations" })

        bmap("n", "go", "<cmd>Telescope lsp_outgoing_calls theme=get_ivy<cr>", { desc = "Outgoing Calls" })
        bmap("n", "gi", "<cmd>Telescope lsp_incoming_calls theme=get_ivy<cr>", { desc = "Incoming Calls" })
    end
    bmap("n", "<C-,>", vim.lsp.codelens.run, { desc = "Run code lens" })
    if sc.codeLensProvider ~= nil and sc.codeLensProvider == true then
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
    bmap({ "n", "x" }, "<C-.>", vim.lsp.buf.code_action)

    if client.server_capabilities.inlayHintProvider then
        vim.lsp.inlay_hint.enable(bufnr, true)
    end
end

require("mason").setup({ ui = { border = Border } })
require("mason-lspconfig").setup({})

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

lspconfig.julials.setup(default)
lspconfig.bashls.setup(default)
lspconfig.fortls.setup(default)
lspconfig.marksman.setup(default)
lspconfig.taplo.setup(default)
lspconfig.asm_lsp.setup(default)
lspconfig.arduino_language_server.setup(default)
lspconfig.teal_ls.setup(default)
lspconfig.nushell.setup(default)
lspconfig.clangd.setup(default)
lspconfig.esbonio.setup(default)


local pyrightcapabilities = vim.lsp.protocol.make_client_capabilities()
pyrightcapabilities.textDocument.publishDiagnostics.tagSupport.valueSet = { 2 }

lspconfig.basedpyright.setup({
    settings = {
        python = {
            analysis = {
                useLibraryCodeForTypes = true,
                diagnosticSeverityOverrides = {
                    reportUnusedVariable = "warning",
                    reportDeprecated = "warning",
                    reportImplicitOverride = "warning",
                    reportMatchNotExhaustive = "warning",
                    reportUnnecessaryTypeIgnoreComment = "warning",
                    reportUnusedExpression = "warning",
                    reportUnusedCoroutine = "warning",
                    reportUnusedClass = "warning",
                    reportUnusedFunction = "warning",
                    diagnosticMode = "workspace",
                },
                typeCheckingMode = "basic",
            },
        },
    },
    capabilities = pyrightcapabilities,
    flags = { debounce_text_changes = 1000 },
})
lspconfig.ruff_lsp.setup(
    {
        capabilities = capabilities,
        flags = { debounce_text_changes = 1000 },
        init_options = {
            settings = {
                args = {
                    "--preview",
                }
            }
        }
    }
)


lspconfig.jsonls.setup({
    capabilities = capabilities,
    flags = { debounce_text_changes = 1000 },
})

lspconfig.yamlls.setup({
    capabilities = capabilities,
    flags = { debounce_text_changes = 1000 },
})

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
        flags = { debounce_text_changes = 1000 },
        settings = {
            ltex = {
                enabled = { "latex", "tex", "bib", "markdown", "rst" },
                checkFrequency = "save",
                language = "en-GB",
                setenceCacheSize = 2000,
                diagnosticSeverity = { MORFOLOGIK_RULE_EN_GB = "hint", default = "info" },
                additionalRules = {
                    enablePickyRules = false,
                    motherTongue = "en-GB",
                },
                dictionary = {},
                disabledRules = { ["en-GB"] = { "OXFORD_SPELLING_Z_NOT_S" } },
                hiddenFalsePositives = {},
                latex = {
                    environments = { Fortran = "ignore", jllisting = "ignore", algorithmic = "ignore" },
                    commands = {
                        ["\\twosubfigures{}{}{}{}{}{}"] = "ignore",
                        ["\\twosubfiguresuncorrected{}{}{}{}{}{}"] = "ignore",
                        ["\\threesubfigures{}{}{}{}{}{}{}{}{}"] = "ignore",
                        ["\\threesubfiguresuncorrected{}{}{}{}{}{}{}{}{}"] = "ignore",
                        ["\\notationnote{}"] = "ignore",
                        ["\\subfile{}"] = "ignore",
                        ["\\Call{}"] = "dummy",
                        ["\\CallText{}"] = "dummy",
                        ["\\glsname{}"] = "dummy",
                        ["\\gls{}"] = "dummy",
                        ["\\glsfirst{}"] = "dummy",
                        ["\\pgls{}"] = "dummy",
                        ["\\ac{}"] = "dummy",
                        ["\\acl{}"] = "dummy",
                        ["\\acs{}"] = "dummy",
                        ["\\acf{}"] = "dummy",
                        ["\\pac{}"] = "dummy",
                        ["\\Pac{}"] = "dummy",
                        ["\\subref{}"] = "dummy",
                        ["\\fullref{}"] = "dummy",
                        ["\\qty{}{}"] = "dummy",
                        ["\\qtyproduct{}{}"] = "dummy",
                        ["\\qtyrange{}{}{}"] = "dummy",
                        ["\\qtylist{}{}"] = "dummy",
                        ["\\unit{}"] = "dummy",
                        ["\\num{}"] = "dummy",
                        ["\\numproduct{}"] = "dummy",
                        ["\\numrange{}{}"] = "dummy",
                        ["\\numlist{}"] = "dummy",
                        ["\\coord{}"] = "dummy",
                        ["\\cmidrule(){}"] = "ignore",
                        ["\\cmidrule[]{}"] = "ignore",
                        ["\\CatchFileDef{}{}{}"] = "ignore",
                        ["\\labelcref{}"] = "dummy",
                        ["\\eqlabel{}"] = "ingore",
                        ["\\fileInput{}"] = "ignore",
                    },
                },
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
        none_ls.builtins.formatting.markdownlint,
        none_ls.builtins.formatting.shellharden,
        none_ls.builtins.formatting.shfmt,
        none_ls.builtins.hover.dictionary.with({ filetypes = { "tex", "markdown" } }),
        none_ls.builtins.hover.printenv,
    },
})

return M
