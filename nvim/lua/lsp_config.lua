----------------------------------------------------------------------------------------------------
--                      _   _   ______    ____   __      __  _____   __  __                       --
--                     | \ | | |  ____|  / __ \  \ \    / / |_   _| |  \/  |                      --
--                     |  \| | | |__    | |  | |  \ \  / /    | |   | \  / |                      --
--                     | . ` | |  __|   | |  | |   \ \/ /     | |   | |\/| |                      --
--                     | |\  | | |____  | |__| |    \  /     _| |_  | |  | |                      --
--                     |_| \_| |______|  \____/      \/     |_____| |_|  |_|                      --
--                                                                                                --
----------------------------------------------------------------------------------------------------
-- Oliver Leete <oliverleete@gmail.com>                                                            --
-- https://github.com/oliver-leete                                                                 --
----------------------------------------------------------------------------------------------------

-- Lsp Settup

local nvim_lsp = require("lspconfig")

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

local function preview_location_callback(_, _, result)
    if result == nil or vim.tbl_isempty(result) then
        return nil
    end
    vim.lsp.util.preview_location(result[1], { focusable = false, border = "single" })
end

function PeekDefinition()
    local params = vim.lsp.util.make_position_params()
    return vim.lsp.buf_request(0, "textDocument/definition", params, preview_location_callback)
end

vim.cmd([[
    sign define DiagnosticSignError text= texthl=DiagnosticSignError linehl= numhl=DiagnosticSignError
    sign define DiagnosticSignWarn text= texthl=DiagnosticSignWarn linehl= numhl=DiagnosticSignWarn
    sign define DiagnosticSignInfo text= texthl=DiagnosticSignInfo linehl= numhl=DiagnosticSignInfo
    sign define DiagnosticSignHint text= texthl=DiagnosticSignHint linehl= numhl=DiagnosticSignHint
]])

vim.diagnostic.config({
    underline = false,
    virtual_text = { severity = "Error" },
    signs = true,
    update_in_insert = false,
    severity_sort = true,
})

local custom_attach = function(client)
    print("LSP: " .. client.name .. " Started")

    vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
        border = "single",
        focusable = false,
    })
    vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
        border = "single",
        focusable = false,
    })

    require("lsp_signature").on_attach({
        bind = true,
        doc_lines = 10,
        floating_window = true,
        fixpos = true,
        hint_enable = false,
        use_lspsaga = false,
        hi_parameter = "IncSearch",
        max_height = 12,
        max_width = 120,
        extra_trigger_chars = { ";" },
        handler_opts = {
            border = "single",
        },
    })
end


require("lspconfig").julials.setup({
    on_attach = custom_attach,
    cmd = { "julia1.6", "--startup-file=no", "--history-file=no", "-e", '    # Load LanguageServer.jl: attempt to load from ~/.julia/environments/nvim-lspconfig\n    # with the regular load path as a fallback\n    ls_install_path = joinpath(\n        get(DEPOT_PATH, 1, joinpath(homedir(), ".julia")),\n        "environments", "nvim-lspconfig"\n    )\n    pushfirst!(LOAD_PATH, ls_install_path)\n    using LanguageServer\n    popfirst!(LOAD_PATH)\n    depot_path = get(ENV, "JULIA_DEPOT_PATH", "")\n    project_path = let\n        dirname(something(\n            ## 1. Finds an explicitly set project (JULIA_PROJECT)\n            Base.load_path_expand((\n                p = get(ENV, "JULIA_PROJECT", nothing);\n                p === nothing ? nothing : isempty(p) ? nothing : p\n            )),\n            ## 2. Look for a Project.toml file in the current working directory,\n            ##    or parent directories, with $HOME as an upper boundary\n            Base.current_project(),\n            ## 3. First entry in the load path\n            get(Base.load_path(), 1, nothing),\n            ## 4. Fallback to default global environment,\n            ##    this is more or less unreachable\n            Base.load_path_expand("@v#.#"),\n        ))\n    end\n    @info "Running language server" VERSION pwd() project_path depot_path\n    server = LanguageServer.LanguageServerInstance(stdin, stdout, project_path, depot_path)\n    server.runlinter = true\n    run(server)\n  ' }
})

require("grammar-guard").init()
require("lspconfig").grammar_guard.setup({
    cmd = { "/home/oleete/.local/share/nvim/lsp_servers/ltex/ltex-ls/bin/ltex-ls" },
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
                environments = { Fortran = "ignore", jllisting = "ignore" },
            },
            dictionary = {},
            disabledRules = { "OXFORD_SPELLING_Z_NOT_S" },
            hiddenFalsePositives = {},
        },
    },
})

require("nvim-lsp-installer").on_server_ready(function(server)
    local opts = {
        on_attach = custom_attach,
        capabilities = capabilities,
        flags = { debounce_text_changes = 500 },
    }
    if server.name == "ltex" then
        return
    elseif server.name == "texlab" then
        opts.root_dir = nvim_lsp.util.root_pattern(".git")
        opts.settings = {
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
        }
    elseif server.name == "sumneko_lua" then
        opts.root_dir = nvim_lsp.util.root_pattern("init.lua")
        opts.settings = {
            Lua = {
                runtime = {
                    version = "LuaJIT",
                    path = vim.split(package.path, ";"),
                },
                diagnostics = {
                    -- enable = false,
                    globals = {
                        "vim",
                        "map",
                        "nmap",
                        "vmap",
                        "xmap",
                        "smap",
                        "omap",
                        "imap",
                        "lmap",
                        "cmap",
                        "tmap",
                        "noremap",
                        "nnoremap",
                        "vnoremap",
                        "xnoremap",
                        "snoremap",
                        "onoremap",
                        "inoremap",
                        "lnoremap",
                        "cnoremap",
                        "tnoremap",
                    },
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
        }
    elseif server.name == "hls" then
        opts.filetypes = { "haskell", "lhaskell" }
        opts.root_dir = nvim_lsp.util.root_pattern("*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml", ".git")
    end
    server:setup(opts)
end)

-- Toggle Lsp
vim.g.diagnostics_active = false
function _G.toggle_diagnostics()
    if vim.g.diagnostics_active then
        vim.g.diagnostics_active = false
        vim.diagnostic.config({
            underline = false,
            virtual_text = { severity = "Error" },
            signs = true,
            update_in_insert = false,
            severity_sort = true,
        })
        vim.cmd([[
        augroup ErrorHover
            autocmd!
        augroup END
        ]])
        -- vim.diagnostic.show()
    else
        vim.g.diagnostics_active = true
        vim.diagnostic.config({
            underline = true,
            virtual_text = {
                prefix = " ",
                spacing = 4,
            },
            signs = true,
            update_in_insert = false,
            severity_sort = true,
        })
        vim.cmd([[
        augroup ErrorHover
            autocmd CursorHold * :lua vim.lsp.diagnostic.show_line_diagnostics({ focusable = false ,  border = 'single' })
        augroup END
        ]])
        -- vim.diagnostic.show()
    end
end

-- Null LS
require("null-ls").setup({
    on_attach = custom_attach,
    diagnostics_format = "[#{c}] #{m} (#{s})",
    sources = {
        require("null-ls").builtins.code_actions.gitsigns.with({ filetype = { "kitty" } }),
        require("null-ls").builtins.formatting.trim_whitespace,
        require("null-ls").builtins.formatting.trim_newlines,
        require("null-ls").builtins.formatting.shfmt,
        require("null-ls").builtins.formatting.stylua,
        require("null-ls").builtins.formatting.fish_indent,
        require("null-ls").builtins.diagnostics.markdownlint,
        require("null-ls").builtins.hover.dictionary.with({ filetype = { "tex", "markdown" } }),
        require("null-ls").builtins.code_actions.refactoring,
        require("null-ls").builtins.diagnostics.chktex,
        -- require("null-ls").builtins.diagnostics.selene,
    },
})
