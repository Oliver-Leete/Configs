local lspconfig = require("lspconfig")

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

vim.api.nvim_set_hl(0, "CursorLineError", { fg = "#E82424", bg = "#363646" })
vim.api.nvim_set_hl(0, "CursorLineWarn", { fg = "#FF9E3B", bg = "#363646" })
vim.api.nvim_set_hl(0, "CursorLineInfo", { fg = "#658494", bg = "#363646" })
vim.api.nvim_set_hl(0, "CursorLineHint", { fg = "#6A9589", bg = "#363646" })
vim.cmd([[
    sign define DiagnosticSignError text= texthl=DiagnosticSignError linehl= numhl= culhl=CursorLineError
    sign define DiagnosticSignWarn text= texthl=DiagnosticSignWarn linehl= numhl= culhl=CursorLineWarn
    sign define DiagnosticSignInfo text= texthl=DiagnosticSignInfo linehl= numhl= culhl=CursorLineInfo
    sign define DiagnosticSignHint text= texthl=DiagnosticSignHint linehl= numhl= culhl=CursorLineHint
]])

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
    border = "single",
})
-- vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
--     border = "single",
-- })

vim.diagnostic.config({
    underline = {
        severity = {
            min = "Warn",
        },
    },
    virtual_text = {
        severity = {
            min = "Warn",
        },
        source = "if_many",
        prefix = "",
    },
    signs = true,
    update_in_insert = false,
    severity_sort = true,
})
Notification_Dict = {}

local lsp_auto = vim.api.nvim_create_augroup("lsp_autocmd", { clear = true })
local custom_attach = function(client, bufnr)
    if client.name ~= "null-ls" then
        if not Notification_Dict[client.name] then
            Notification_Dict[client.name] = true
            ---@diagnostic disable-next-line: redundant-parameter
            vim.notify(client.name .. " started", "info", {
                title = "LSP",
                on_close = function() Notification_Dict[client.name] = false end,
            })
        end
    end
    if client.server_capabilities.documentSymbolProvider then
        require("nvim-navic").attach(client, bufnr)
    end

    local bmap = function(mode, key, action) Map(mode, key, action, { buffer = bufnr }) end
    -- LSP Binding Override
    if client.name ~= "null-ls" then
        bmap("n", "gd", "<cmd>Telescope lsp_definitions theme=get_ivy<cr>")
        bmap("n", "gs", "<cmd>Telescope lsp_workspace_symbols theme=get_ivy<cr>")
        bmap("n", "gS", "<cmd>Telescope lsp_document_symbols theme=get_ivy<cr>")
        bmap("n", "gr", "<cmd>Telescope lsp_references theme=get_ivy<cr>")
        bmap("n", "gI", "<cmd>Telescope lsp_implementations theme=get_ivy<cr>")
        bmap("n", "gD", "<cmd>Telescope lsp_type_definitions theme=get_ivy<cr>")
        bmap("n", "go", vim.lsp.buf.outgoing_calls)
        bmap("n", "gi", vim.lsp.buf.incoming_calls)

        bmap("n", "KK", vim.lsp.buf.hover)
    end
    if client.server_capabilities.codeLensProvider ~= nil then
        bmap("n", "<C-,>", vim.lsp.codelens.run)
        bmap("n", "<leader>,", vim.lsp.codelens.run)
        vim.api.nvim_create_autocmd(
            "CursorHold",
            { callback = vim.lsp.codelens.refresh, buffer = bufnr, group = lsp_auto }
        )
    end
    if client.server_capabilities.codeActionProvider then
        bmap("n", "<C-.>", vim.lsp.buf.code_action)
        bmap("n", "<leader>.", vim.lsp.buf.code_action)
        bmap("x", "<C-.>", vim.lsp.buf.range_code_action)
        bmap("x", "<leader>.", vim.lsp.buf.range_code_action)
    end

    if client.server_capabilities.signatureHelpProvider then
        require('lsp-overloads').setup(client, {})
    end
end

require("grammar-guard").init()
require("nvim-lsp-installer").setup({})

local default = {
    on_attach = custom_attach,
    capabilities = capabilities,
    flags = { debounce_text_changes = 500 },
}

lspconfig.julials.setup(default)
lspconfig.bashls.setup(default)
lspconfig.fortls.setup(default)
lspconfig.jedi_language_server.setup(default)

lspconfig.sourcery.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
    flags = { debounce_text_changes = 500 },
    init_options = {
        token = "user_ncjcsKxRD7LGXBwHUwLWu7iSmWgu81zaRMbDjwNZqfGkUhRaDVMuZ9pwVyA",
        extension_version = "vim.lsp",
        editor_version = "vim",
    },
})

lspconfig.jsonls.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
    flags = { debounce_text_changes = 500 },
    settings = {
        json = {
            schemas = require("schemastore").json.schemas(),
        },
    },
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
    flags = { debounce_text_changes = 500 },
    settings = {
        yaml = {
            schemaStore = {
                enable = yaml_schemas,
            }
        }
    }
})

lspconfig.sumneko_lua.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
    flags = { debounce_text_changes = 500 },
    root_dir = lspconfig.util.root_pattern("init.lua"),
    settings = {
        Lua = {
            runtime = {
                version = "LuaJIT",
                path = vim.split(package.path, ";"),
            },
            diagnostics = {
                -- enable = false,
                globals = { "vim" },
            },
            workspace = {
                library = {
                    [vim.fn.expand("$VIMRUNTIME/lua")] = true,
                    [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
                },
                maxPreload = 3000,
                preloadFileSize = 500,
            },
            telemetry = {
                enable = false,
            },
        },
    },
})

lspconfig.texlab.setup({
    on_attach = custom_attach,
    -- capabilities = capabilities,
    flags = { debounce_text_changes = 500 },
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

lspconfig.hls.setup({
    on_attach = custom_attach,
    -- capabilities = capabilities,
    flags = { debounce_text_changes = 500 },
    root_dir = lspconfig.util.root_pattern("*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml"),
})

require("clangd_extensions").setup({
    server = {
        on_attach = custom_attach,
        capabilities = capabilities,
        flags = { debounce_text_changes = 500 },
    },
})

require("rust-tools").setup({
    server = {
        on_attach = custom_attach,
        capabilities = capabilities,
        flags = { debounce_text_changes = 500 },
        standalone = true,
    },
    tools = {
        executor = require("rust-tools/executors").toggleterm,
    },
    dap = {
        adapter = require("rust-tools.dap").get_codelldb_adapter(
            "/home/oleete/.local/share/nvim/dapinstall/codelldb/extension/adapter/codelldb",
            "/home/oleete/.local/share/nvim/dapinstall/codelldb/extension/lldb/lib/liblldb.so"
        ),
    },
})

lspconfig.grammar_guard.setup({
    cmd = { "/home/oleete/.local/share/nvim/lsp_servers/ltex/ltex-ls/bin/ltex-ls" },
    on_attach = custom_attach,
    flags = { debounce_text_changes = 500 },
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
                },
            },
            dictionary = { ["en-GB"] = { "ANSYS", "UPF" } },
            disabledRules = { ["en-GB"] = { "OXFORD_SPELLING_Z_NOT_S" } },
            hiddenFalsePositives = {},
        },
    },
})

local null_ls = require("null-ls")
-- Null LS
require("null-ls").setup({
    on_attach = custom_attach,
    diagnostics_format = "[#{c}] #{m} (#{s})",
    sources = {
        null_ls.builtins.code_actions.gitsigns.with({ filetypes = { "kitty" } }),
        null_ls.builtins.formatting.trim_whitespace,
        null_ls.builtins.formatting.trim_newlines,
        null_ls.builtins.formatting.shfmt,
        null_ls.builtins.formatting.shellharden,
        -- null_ls.builtins.formatting.stylua,
        null_ls.builtins.formatting.fish_indent,
        null_ls.builtins.formatting.latexindent,
        null_ls.builtins.diagnostics.markdownlint,
        null_ls.builtins.diagnostics.chktex,
        null_ls.builtins.hover.dictionary.with({ filetypes = { "tex", "markdown" } }),
        null_ls.builtins.code_actions.refactoring,
        null_ls.builtins.diagnostics.gitlint,
    },
})

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
