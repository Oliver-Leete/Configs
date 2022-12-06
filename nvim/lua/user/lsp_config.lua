require("neodev").setup({})
local lspconfig = require("lspconfig")
local lsp_selection_range = require('lsp-selection-range')
-- require("inc_rename").setup()

vim.api.nvim_set_hl(0, "CursorLineError", { fg = "#E82424", bg = "#363646" })
vim.api.nvim_set_hl(0, "CursorLineWarn", { fg = "#FF9E3B", bg = "#363646" })
vim.api.nvim_set_hl(0, "CursorLineInfo", { fg = "#658494", bg = "#363646" })
vim.api.nvim_set_hl(0, "CursorLineHint", { fg = "#6A9589", bg = "#363646" })
vim.fn.sign_define("DiagnosticSignError", { text = " ", texthl = "DiagnosticSignError", culhl = "CursorLineError" })
vim.fn.sign_define("DiagnosticSignWarn", { text = " ", texthl = "DiagnosticSignWarn", culhl = "CursorLineWarn" })
vim.fn.sign_define("DiagnosticSignInfo", { text = " ", texthl = "DiagnosticSignInfo", culhl = "CursorLineInfo" })
vim.fn.sign_define("DiagnosticSignHint", { text = " ", texthl = "DiagnosticSignHint", culhl = "CursorLineHint" })

vim.diagnostic.config({
    underline = { severity = { min = "Warn", }, },
    virtual_text = { severity = { min = "Warn", }, source = "if_many", prefix = " ", },
    signs = true,
    update_in_insert = false,
    severity_sort = true,
})

local lsp_auto = vim.api.nvim_create_augroup("lsp_autocmd", { clear = true })

local custom_attach = function(client, bufnr)
    local sc = client.server_capabilities
    if sc.documentSymbolProvider and not client.name == "jedi_language_server" then
        require("nvim-navic").attach(client, bufnr)
    end
    lsp_selection_range.setup({})

    local bmap = function(mode, key, action) Map(mode, key, action, { buffer = bufnr }) end
    -- LSP Binding Override
    if client.name ~= "null-ls" then
        bmap("n", "gd", "<cmd>Glance definitions<cr>")
        bmap("n", "gr", "<cmd>Glance references<cr>")
        bmap("n", "gD", "<cmd>Glance type_definitions<cr>")
        bmap("n", "gI", "<cmd>Glance implementations<cr>")
        bmap("n", "gs", "<cmd>Telescope lsp_workspace_symbols theme=get_ivy<cr>")
        bmap("n", "gS", "<cmd>Telescope lsp_document_symbols theme=get_ivy<cr>")
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
    if sc.codeActionProvider then
        bmap({ "n", "x" }, "<C-.>", vim.lsp.buf.code_action)
    end
end

require("mason").setup({})
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
lspconfig.pyright.setup(default)
lspconfig.marksman.setup(default)
lspconfig.taplo.setup(default)

lspconfig.jedi_language_server.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
    flags = { debounce_text_changes = 1000 },
    init_options = { diagnostics = { enable = false, }, }
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

lspconfig.sumneko_lua.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
    flags = { debounce_text_changes = 1000 },
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
                onEdit = false,
                onOpenAndSave = false,
            },
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
                    ["\\threesubfigures{}{}{}{}{}{}{}{}{}"] = "ignore",
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
        -- null_ls.builtins.code_actions.gitsigns,
        -- null_ls.builtins.code_actions.refactoring,
        null_ls.builtins.diagnostics.chktex,
        null_ls.builtins.diagnostics.fish,
        -- null_ls.builtins.diagnostics.flake8,
        null_ls.builtins.diagnostics.gitlint,
        null_ls.builtins.diagnostics.jsonlint,
        null_ls.builtins.diagnostics.markdownlint.with({ extra_args = { "--disable", "MD013", "MD046", "MD009" } }),
        null_ls.builtins.diagnostics.pydocstyle,
        null_ls.builtins.formatting.black,
        null_ls.builtins.formatting.cbfmt,
        null_ls.builtins.formatting.fish_indent,
        null_ls.builtins.formatting.isort,
        null_ls.builtins.formatting.jq.with({ extra_args = { "--indent", "4" } }),
        null_ls.builtins.formatting.latexindent,
        null_ls.builtins.formatting.markdownlint,
        null_ls.builtins.formatting.shellharden,
        null_ls.builtins.formatting.shfmt,
        null_ls.builtins.formatting.trim_newlines,
        null_ls.builtins.formatting.trim_whitespace,
        null_ls.builtins.hover.dictionary.with({ filetypes = { "tex", "markdown" } }),
        null_ls.builtins.hover.printenv,
    },
})

require('ufo').setup()

-- Non lsp diagnostics
QfDiag = vim.api.nvim_create_namespace("qfDiag")
QfToDiagGroup = vim.api.nvim_create_augroup("qfToDiag", { clear = true })

local function UpdateDiagnostics(diagnostics, namespace)
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

QFtoDiag = function()
    local qf = vim.diagnostic.fromqflist(vim.fn.getqflist())
    UpdateDiagnostics(qf, QfDiag)
end
vim.api.nvim_create_autocmd("QuickFixCmdPost", { pattern = "*", callback = QFtoDiag, group = QfToDiagGroup })
vim.api.nvim_create_autocmd("User", { pattern = "VimtexEventCompileFailed", callback = QFtoDiag, group = QfToDiagGroup })
vim.api.nvim_create_autocmd("User", { pattern = "VimtexEventCompileSuccess", callback = QFtoDiag, group = QfToDiagGroup })
