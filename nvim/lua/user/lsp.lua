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
        "jq",
        "json-lsp",
        "jsonlint",
        "julia-lsp",
        "ltex-ls",
        "lua-language-server",
        "markdownlint",
        "marksman",
        "pyright",
        "ruff-lsp",
        "rust-analyzer",
        "shellharden",
        "shfmt",
        "sourcery",
        "taplo",
        "teal-language-server",
        "texlab",
        "yaml-language-server",
    },
})

require("neodev").setup({})
local lspconfig = require("lspconfig")

require("lspconfig.ui.windows").default_options.border = Border

vim.fn.sign_define("DiagnosticSignError",
    { text = " ", texthl = "DiagnosticSignError", culhl = "DiagnosticSignErrorCur" })
vim.fn.sign_define("DiagnosticSignWarn", { text = " ", texthl = "DiagnosticSignWarn", culhl = "DiagnosticSignWarnCur" })
vim.fn.sign_define("DiagnosticSignInfo", { text = " ", texthl = "DiagnosticSignInfo", culhl = "DiagnosticSignInfoCur" })
vim.fn.sign_define("DiagnosticSignHint", { text = "󰅽 ", texthl = "DiagnosticSignHint", culhl = "DiagnosticSignHintCur" })

local default_diagnostic_config = {
    underline = { severity = { min = "Warn", }, },
    virtual_text = { severity = { min = "Warn", }, source = "if_many", prefix = " ", },
    signs = { priority = 6 },
    update_in_insert = false,
    severity_sort = true,
    virtual_lines = false,
}

vim.api.nvim_set_hl(0, "LspInlayHint", { link = "NvimDapVirtualText" })

vim.diagnostic.config(default_diagnostic_config)

local lsp_auto = vim.api.nvim_create_augroup("lsp_autocmd", { clear = true })

local custom_attach = function(client, bufnr)
    local sc = client.server_capabilities
    local bmap = function(mode, key, action) Map(mode, key, action, { buffer = bufnr }) end

    if client.name == "ruff" then
        sc.renameProvider = false
        sc.definitionProvider = false
        sc.referencesProvider = false
    elseif client.name == "texlab" then
        sc.documentFormattingProvider = false
    end

    if sc.documentSymbolProvider then
        require("nvim-navic").attach(client, bufnr)
    end

    -- LSP Binding Override
    if client.name ~= "null-ls" then
        bmap("n", "gd", "<cmd>Glance definitions<cr>")
        bmap("n", "gr", "<cmd>Glance references<cr>")
        bmap("n", "gD", "<cmd>Glance type_definitions<cr>")
        bmap("n", "gI", "<cmd>Glance implementations<cr>")

        bmap("n", "go", "<cmd>Telescope lsp_outgoing_calls theme=get_ivy<cr>")
        bmap("n", "gi", "<cmd>Telescope lsp_incoming_calls theme=get_ivy<cr>")
    end
    if sc.codeLensProvider ~= nil and sc.codeLensProvider == true then
        bmap("n", "<C-,>", vim.lsp.codelens.run)
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
lspconfig.clangd.setup(default)
lspconfig.esbonio.setup(default)
lspconfig.hls.setup({
    flags = { debounce_text_changes = 1000 },
    settings = {
        haskell = {
            formattingProvider = "stylish-haskell",
            checkProject = true,
        }
    }
})

local pyrightcapabilities = vim.lsp.protocol.make_client_capabilities()
pyrightcapabilities.textDocument.publishDiagnostics.tagSupport.valueSet = { 2 }

lspconfig.pyright.setup({
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
        none_ls.builtins.diagnostics.gitlint,
        none_ls.builtins.diagnostics.jsonlint,
        none_ls.builtins.diagnostics.markdownlint.with({ extra_args = { "--disable", "MD013", "MD046", "MD009" } }),
        none_ls.builtins.formatting.bibclean,
        none_ls.builtins.formatting.fish_indent,
        none_ls.builtins.formatting.jq.with({ extra_args = { "--indent", "4" } }),
        none_ls.builtins.formatting.latexindent.with({ extra_args = { "-l=.latexindent.yaml" } }),
        none_ls.builtins.formatting.markdownlint,
        none_ls.builtins.formatting.shellharden,
        none_ls.builtins.formatting.shfmt,
        none_ls.builtins.formatting.trim_newlines,
        none_ls.builtins.formatting.trim_whitespace,
        none_ls.builtins.formatting.ruff.with({
            args = { "--fix", "-e", "-n", "--fixable", "I001", "--stdin-filename", "$FILENAME", "-" } }), -- only format import block
        none_ls.builtins.hover.dictionary.with({ filetypes = { "tex", "markdown" } }),
        none_ls.builtins.hover.printenv,
    },
})

local toggled_diagnostic_config = {
    underline = true,
    virtual_text = false,
    signs = { priority = 6 },
    update_in_insert = false,
    severity_sort = true,
    virtual_lines = { only_current_line = true },
}

require("lsp_lines").setup()

local clear_preview_inline = function()
    vim.diagnostic.config(default_diagnostic_config)
end

M.preview_diagnostics_inline = function()
    local bufnr = vim.api.nvim_get_current_buf()

    vim.diagnostic.config(toggled_diagnostic_config)

    vim.api.nvim_create_autocmd({ 'CursorMoved', 'InsertEnter' }, {
        buffer = bufnr,
        desc = 'Clear diagnostics inline preview',
        callback = function()
            clear_preview_inline()
        end,
        once = true,
    })
end

return M
