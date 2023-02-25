require("neodev").setup({})
local lspconfig = require("lspconfig")
local lsp_selection_range = require('lsp-selection-range')
-- require("inc_rename").setup()

require("lspconfig.ui.windows").default_options.border = Border

vim.fn.sign_define("DiagnosticSignError",
    { text = " ", texthl = "DiagnosticSignError", culhl = "DiagnosticSignErrorCur" })
vim.fn.sign_define("DiagnosticSignWarn", { text = " ", texthl = "DiagnosticSignWarn", culhl = "DiagnosticSignWarnCur" })
vim.fn.sign_define("DiagnosticSignInfo", { text = " ", texthl = "DiagnosticSignInfo", culhl = "DiagnosticSignInfoCur" })
vim.fn.sign_define("DiagnosticSignHint", { text = " ", texthl = "DiagnosticSignHint", culhl = "DiagnosticSignHintCur" })

vim.diagnostic.config({
    underline = { severity = { min = "Warn", }, },
    virtual_text = { severity = { min = "Warn", }, source = "if_many", prefix = " ", },
    signs = { priority = 6 },
    update_in_insert = false,
    severity_sort = true,
})

local lsp_auto = vim.api.nvim_create_augroup("lsp_autocmd", { clear = true })

local custom_attach = function(client, bufnr)
    local sc = client.server_capabilities
    local bmap = function(mode, key, action) Map(mode, key, action, { buffer = bufnr }) end

    if client.name == "pylsp" then
        sc.renameProvider = false
        sc.definitionProvider = false
        sc.referencesProvider = false
        -- sc.documentSymbolProvider = false
    end

    if sc.documentSymbolProvider and client.name ~= "pyright" then
        require("nvim-navic").attach(client, bufnr)
    end

    lsp_selection_range.setup({})

    -- LSP Binding Override
    if client.name ~= "null-ls" then
        bmap("n", "gd", "<cmd>Glance definitions<cr>")
        bmap("n", "gr", "<cmd>Glance references<cr>")
        bmap("n", "gD", "<cmd>Glance type_definitions<cr>")
        bmap("n", "gI", "<cmd>Glance implementations<cr>")

        bmap("n", "go", "<cmd>Telescope lsp_outgoing_calls theme=get_ivy<cr>")
        bmap("n", "gi", "<cmd>Telescope lsp_incoming_calls theme=get_ivy<cr>")

        bmap("n", "Gd", "<cmd>Telescope lsp_definitions theme=get_ivy<cr>")
        bmap("n", "Gr", "<cmd>Telescope lsp_references theme=get_ivy<cr>")
        bmap("n", "GI", "<cmd>Telescope lsp_implementations theme=get_ivy<cr>")
        bmap("n", "GD", "<cmd>Telescope lsp_type_definitions theme=get_ivy<cr>")

        bmap("n", "KK", vim.lsp.buf.hover)

        bmap("n", "mm", require("lsp-selection-range").trigger)
        bmap("x", "mm", require("lsp-selection-range").expand)
    end
    if sc.codeLensProvider ~= nil then
        bmap("n", "<C-,>", vim.lsp.codelens.run)
        vim.api.nvim_create_autocmd(
            "CursorHold",
            { callback = vim.lsp.codelens.refresh, buffer = bufnr, group = lsp_auto }
        )
    end
    bmap({ "n", "x" }, "<C-.>", vim.lsp.buf.code_action)
end

require("mason").setup({ ui = { border = Border } })
require("mason-lspconfig").setup({})


local capabilities = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())
capabilities.textDocument.foldingRange = {
    dynamicRegistration = false,
    lineFoldingOnly = true
}
capabilities = lsp_selection_range.update_capabilities(capabilities)

local default = {
    on_attach = custom_attach,
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

lspconfig.pyright.setup(default)
require("lspconfig").pylsp.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
    flags = { debounce_text_changes = 1000 },
    settings = {
        pylsp = {
            plugins = {
                pydocstyle = {
                    enabled = false,
                    addIgnore = { "D101", "D102", "D103", "D107", "D203" },
                    convention = "numpy",
                },
                pycodestyle = {
                    enabled = false,
                    ignore = { "E501", "W503" }
                },
                pyflakes = {
                    enabled = false,
                },
                rope_completion = { enabled = false },
                jedi_completion = { enabled = false },
                ruff = {
                    enabled = true,
                    lineLength = 120,
                },
            },
        },
    },
})
lspconfig.sourcery.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
    flags = { debounce_text_changes = 1000 },
    init_options = {
        token = "user_ncjcsKxRD7LGXBwHUwLWu7iSmWgu81zaRMbDjwNZqfGkUhRaDVMuZ9pwVyA",
        extension_version = "vim.lsp",
        editor_version = "vim",
    },
})

lspconfig.jsonls.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
    flags = { debounce_text_changes = 1000 },
    settings = { json = { schemas = require("schemastore").json.schemas(), }, },
})

-- HACK: replace once https://github.com/b0o/SchemaStore.nvim/pull/10 merged
local json_schemas = require('schemastore').json.schemas {}
local yaml_schemas = {}
vim.tbl_map(function(schema)
    yaml_schemas[schema.url] = schema.fileMatch
end, json_schemas)

lspconfig.yamlls.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
    flags = { debounce_text_changes = 1000 },
    settings = { yaml = { schemaStore = { enable = yaml_schemas, } } }
})

lspconfig.lua_ls.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
    flags = { debounce_text_changes = 1000 },
    settings = {
        Lua = {
            runtime = {
                version = 'LuaJIT',
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
    on_attach = custom_attach,
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

local ht = require('haskell-tools')
ht.setup({
    hls = {
        on_attach = custom_attach,
        flags = { debounce_text_changes = 1000 },
        root_dir = lspconfig.util.root_pattern("*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml"),
        settings = {
            haskell = {
                formattingProvider = "stylish-haskell",
                checkProject = true,
            }
        }
    }
})

local clangd_cap = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())
clangd_cap.offsetEncoding = "utf-8"
require("clangd_extensions").setup({
    server = {
        on_attach = custom_attach,
        capabilities = clangd_cap,
        flags = { debounce_text_changes = 1000 },
    },
})

require("rust-tools").setup({
    server = {
        on_attach = custom_attach,
        capabilities = capabilities,
        flags = { debounce_text_changes = 1000 },
        standalone = true,
        settings = {
            ["rust-analyzer"] = {
                checkOnSave = {
                    command = { "cargo", "clippy" },
                },
            },
        }
    },
    tools = {
        executor = require("rust-tools/executors").overseer,
    },
    dap = {
        adapter = require("rust-tools.dap").get_codelldb_adapter(
            "/home/oleete/.local/share/nvim/mason/bin/codelldb",
            "/home/oleete/.local/share/nvim/mason/packages/codelldb/extension/lldb/lib/liblldb.so"
        ),
    },
})

require("lspconfig").ltex.setup({
    capabilities = capabilities,
    flags = { debounce_text_changes = 1000 },
    on_attach = function(client, bufnr)
        require("ltex_extra").setup({
            load_langs = { "en-GB" },
            init_check = true,
            path = nil,
            log_level = "none",
        })
        custom_attach(client, bufnr)
    end,
    settings = {
        ltex = {
            enabled = { "latex", "tex", "bib", "markdown" },
            checkFrequency = "save",
            language = "en-GB",
            setenceCacheSize = 2000,
            diagnosticSeverity = { MORFOLOGIK_RULE_EN_GB = "hint", default = "info" },
            additionalRules = {
                enablePickyRules = false,
                motherTongue = "en-GB",
            },
            latex = {
                environments = { Fortran = "ignore", jllisting = "ignore", algorithmic = "ignore" },
                commands = {
                    ["\\twosubfigures{}{}{}{}{}{}"] = "ignore",
                    ["\\twosubfiguresuncorrected{}{}{}{}{}{}"] = "ignore",
                    ["\\threesubfigures{}{}{}{}{}{}{}{}{}"] = "ignore",
                    ["\\threesubfiguresuncorrected{}{}{}{}{}{}{}{}{}"] = "ignore",
                    ["\\subfile{}"] = "ignore",
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
                    ["\\SI{}{}"] = "dummy",
                },
            },
            dictionary = {},
            disabledRules = { ["en-GB"] = { "OXFORD_SPELLING_Z_NOT_S" } },
            hiddenFalsePositives = {},
        },
    },
})

local null_ls = require("null-ls")
-- Null LS
require("null-ls").setup({
    diagnostics_format = "[#{c}] #{m} (#{s})",
    sources = {
        null_ls.builtins.code_actions.gitrebase,
        null_ls.builtins.code_actions.refactoring.with({ disabled_filetypes = { "python" } }),
        null_ls.builtins.diagnostics.fish,
        null_ls.builtins.diagnostics.gitlint,
        null_ls.builtins.diagnostics.jsonlint,
        null_ls.builtins.diagnostics.markdownlint.with({ extra_args = { "--disable", "MD013", "MD046", "MD009" } }),
        null_ls.builtins.formatting.fish_indent,
        null_ls.builtins.formatting.jq.with({ extra_args = { "--indent", "4" } }),
        null_ls.builtins.formatting.markdownlint,
        null_ls.builtins.formatting.shellharden,
        null_ls.builtins.formatting.shfmt,
        null_ls.builtins.formatting.trim_newlines,
        null_ls.builtins.formatting.trim_whitespace,
        null_ls.builtins.hover.dictionary.with({ filetypes = { "tex", "markdown" } }),
        null_ls.builtins.hover.printenv,
        require("null-ls-embedded").nls_source,
    },
})

-- Non lsp diagnostics
QfDiag = vim.api.nvim_create_namespace("qfDiag")
QfToDiagGroup = vim.api.nvim_create_augroup("qfToDiag", { clear = true })

local update_diagnostics = function(diagnostics, namespace)
    vim.diagnostic.reset(namespace)
    local buffers = {}
    local tmp = {}
    for i, item in pairs(diagnostics) do
        if (tmp[item.bufnr] ~= nil) then
            table.insert(buffers, item.bufnr)
        end
        tmp[item.bufnr] = i
    end

    for _, buffer in pairs(buffers) do
        local diag = {}
        for _, d in pairs(diagnostics) do
            if d.bufnr == buffer then
                table.insert(diag, d)
            end
        end
        vim.diagnostic.set(namespace, buffer, diag)
    end
end

local qf_to_diag = function()
    local qf = vim.diagnostic.fromqflist(vim.fn.getqflist())
    update_diagnostics(qf, QfDiag)
end
vim.api.nvim_create_autocmd("QuickFixCmdPost", { pattern = "*", callback = qf_to_diag, group = QfToDiagGroup })
vim.api.nvim_create_autocmd("User",
    { pattern = "VimtexEventCompileFailed", callback = qf_to_diag, group = QfToDiagGroup })
vim.api.nvim_create_autocmd("User",
    { pattern = "VimtexEventCompileSuccess", callback = qf_to_diag, group = QfToDiagGroup })
