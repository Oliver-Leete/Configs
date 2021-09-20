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
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

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
    sign define DiagnosticSignError text= texthl=DiagnosticSignError linehl= numhl=DiagnosticSignError
    sign define DiagnosticSignWarning text= texthl=DiagnosticSignWarning linehl= numhl=DiagnosticSignWarning
    sign define DiagnosticSignInformation text= texthl=DiagnosticSignInformation linehl= numhl=DiagnosticSignInformation
    sign define DiagnosticSignHint text= texthl=DiagnosticSignHint linehl= numhl=DiagnosticSignHint
]])

vim.diagnostic.config({
    underline = false,
    virtual_text = { severity = "Error"},
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

    require("illuminate").on_attach(client)
    vim.cmd([[autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb({sign={priority=7}})]])
end

require("lspconfig").hls.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
    flags = { debounce_text_changes = 500 },
    cmd = { "haskell-language-server-wrapper", "--lsp" },
    filetypes = { "haskell", "lhaskell" },
    root_dir = nvim_lsp.util.root_pattern("*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml", ".git"),
    lspinfo = function(cfg)
        -- return "specific"
        if cfg.settings.languageServerHaskell.logFile or false then
            return "logfile: " .. cfg.settings.languageServerHaskell.logFile
        end
        return ""
    end,
})

require("lspconfig").julials.setup({
    on_attach = custom_attach,
    -- capabilities = capabilities,
    -- flags = { debounce_text_changes = 500 },
})

require("lspinstall").setup()
local servers = require("lspinstall").installed_servers()
for _, server in pairs(servers) do
    if server == "latex" then
        require("lspconfig").texlab.setup({
            on_attach = custom_attach,
            capabilities = capabilities,
            flags = { debounce_text_changes = 500 },
            root_dir = nvim_lsp.util.root_pattern(".git"),
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
    elseif server == "lua" then
        require("lspconfig").lua.setup({
            on_attach = custom_attach,
            capabilities = capabilities,
            flags = { debounce_text_changes = 500 },
            cmd = {
                "/home/oleete/.local/share/nvim/lspinstall/lua/sumneko-lua-language-server",
                "-E",
                "/home/oleete/.local/share/nvim/lspinstall/lua/sumneko-lua/extension/server/bin/linux/sumneko-lua-language-server",
            },
            root_dir = nvim_lsp.util.root_pattern("init.vim", "init.lua"),
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
                        maxPreload = 2000,
                        preloadFileSize = 500,
                    },
                    telemetry = {
                        enable = false,
                    },
                },
            },
        })
    elseif server == "diagnosticls" then
        -- require("lspconfig").diagnosticls.setup({
        --     on_attach=custom_attach,
        -- capabilities = capabilities,
        --     filetypes = {"tex"},
        --     initializationOptions = {},
        -- })
    else
        require("lspconfig")[server].setup({
            on_attach = custom_attach,
            capabilities = capabilities,
            flags = { debounce_text_changes = 500 },
        })
    end
end

require("lspkind").init({
    with_text = true,
    symbol_map = {
        Text = "",
        Method = "ƒ",
        Function = "",
        Constructor = "",
        Variable = "",
        Class = "",
        Interface = "ﰮ",
        Module = "",
        Property = "",
        Unit = "",
        Value = "",
        Enum = "了",
        Keyword = "",
        Snippet = "﬌",
        Color = "",
        File = "",
        Folder = "",
        EnumMember = "",
        Constant = "",
        Struct = "",
    },
})

-- LTEX
local configs = require("lspconfig/configs")

local function readFiles(files)
    local dict = {}
    for _, file in pairs(files) do
        local f = io.open(file, "r")
        for l in f:lines() do
            table.insert(dict, l)
        end
    end
    return dict
end

local function findLtexLang()
    local buf_clients = vim.lsp.buf_get_clients()
    for _, client in pairs(buf_clients) do
        if client.name == "ltex" then
            return client.config.settings.ltex.language
        end
    end
end

local function findLtexFiles(filetype, value)
    local buf_clients = vim.lsp.buf_get_clients()
    for _, client in pairs(buf_clients) do
        if client.name == "ltex" then
            local files = nil
            if filetype == "dictionary" then
                files = client.config.dictionary_files[value or findLtexLang()]
            elseif filetype == "disable" then
                files = client.config.disabledrules_files[value or findLtexLang()]
            elseif filetype == "falsePositive" then
                files = client.config.falsepositive_files[value or findLtexLang()]
            end

            if files then
                return files
            else
                return nil
            end
        end
    end
end

local function updateConfig(lang, configtype)
    local buf_clients = vim.lsp.buf_get_clients()
    local client = nil
    for _, lsp in pairs(buf_clients) do
        if lsp.name == "ltex" then
            client = lsp
        end
    end

    if client then
        if configtype == "dictionary" then
            -- if client.config.settings.ltex.dictionary then
            client.config.settings.ltex.dictionary = {
                [lang] = readFiles(client.config.dictionary_files[lang]),
            }
            return client.notify("workspace/didChangeConfiguration", client.config.settings)
            -- else
            -- return vim.notify("Error when reading dictionary config, check it")
            -- end
        elseif configtype == "disable" then
            if client.config.settings.ltex.disabledRules then
                client.config.settings.ltex.disabledRules = {
                    [lang] = readFiles(client.config.disabledrules_files[lang]),
                }
                return client.notify("workspace/didChangeConfiguration", client.config.settings)
            else
                return vim.notify("Error when reading disabledRules config, check it")
            end
        elseif configtype == "falsePositive" then
            if client.config.settings.ltex.hiddenFalsePositives then
                client.config.settings.ltex.hiddenFalsePositives = {
                    [lang] = readFiles(client.config.falsepositive_files[lang]),
                }
                return client.notify("workspace/didChangeConfiguration", client.config.settings)
            else
                return vim.notify("Error when reading hiddenFalsePositives config, check it")
            end
        end
    else
        return nil
    end
end

local function addToFile(filetype, lang, file, value)
    file = io.open(file[#file - 0], "a+") -- add only to last file defined.
    if file then
        file:write(value .. "\n")
        file:close()
    else
        return print("Failed insert %q", value)
    end
    if filetype == "dictionary" then
        return updateConfig(lang, "dictionary")
    elseif filetype == "disable" then
        return updateConfig(lang, "disable")
    elseif filetype == "falsePositive" then
        return updateConfig(lang, "falsePositive")
    end
end

local function addTo(filetype, lang, file, value)
    local dict = readFiles(file)
    for _, v in ipairs(dict) do
        if v == value then
            return nil
        end
    end
    return addToFile(filetype, lang, file, value)
end

configs.ltex = {
    default_config = {
        cmd = { "/home/oleete/.local/ltex-ls-13.0.0/bin/ltex-ls" },
        filetypes = { "tex", "markdown" },
        dictionary_files = { ["en-GB"] = { vim.fn.getcwd() .. "/dictionary.ltex" } },
        disabledrules_files = { ["en-GB"] = { vim.fn.getcwd() .. "/disable.ltex" } },
        falsepositive_files = { ["en-GB"] = { vim.fn.getcwd() .. "/false.ltex" } },
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
                disabledRules = {},
                hiddenFalsePositives = {},
            },
        },
        on_attach = function(client)
            -- local lang = client.config.settings.ltex.language
            for lang, _ in ipairs(client.config.dictionary_files) do --
                updateConfig(lang, "dictionary")
                updateConfig(lang, "disable")
                updateConfig(lang, "falsePositive")
            end
        end,
    },
}
--
-- https://github.com/neovim/nvim-lspconfig/issues/858 can't intercept,
-- override it then.
local orig_execute_command = vim.lsp.buf.execute_command
vim.lsp.buf.execute_command = function(command)
    if command.command == "_ltex.addToDictionary" then
        local arg = command.arguments[1].words -- can I really access like this?
        for lang, words in pairs(arg) do
            for _, word in ipairs(words) do
                local filetype = "dictionary"
                addTo(filetype, lang, findLtexFiles(filetype, lang), word)
            end
        end
    elseif command.command == "_ltex.disableRules" then
        local arg = command.arguments[1].ruleIds -- can I really access like this?
        for lang, rules in pairs(arg) do
            for _, rule in ipairs(rules) do
                local filetype = "disable"
                addTo(filetype, lang, findLtexFiles(filetype, lang), rule)
            end
        end
    elseif command.command == "_ltex.hideFalsePositives" then
        local arg = command.arguments[1].falsePositives -- can I really access like this?
        for lang, rules in pairs(arg) do
            for _, rule in ipairs(rules) do
                local filetype = "falsePositive"
                addTo(filetype, lang, findLtexFiles(filetype, lang), rule)
            end
        end
    else
        orig_execute_command(command)
    end
end

require("lspconfig").ltex.setup({
    on_attach = custom_attach,
    capabilities = capabilities,
    root_dir = nvim_lsp.util.root_pattern(".git"),
})

-- Toggle Lsp
vim.g.diagnostics_active = false
function _G.toggle_diagnostics()
    if vim.g.diagnostics_active then
        vim.g.diagnostics_active = false
        vim.diagnostic.config({
            underline = false,
            virtual_text = { severity = "Error"},
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
require("null-ls").config({
    sources = {
        -- require("null-ls").builtins.code_actions.gitsigns.with({ filetype = { "jl" } }),
        require("null-ls").builtins.formatting.trim_whitespace.with({ filetypes = { "markdown", "tex" } }),
        require("null-ls").builtins.formatting.stylua,
        require("null-ls").builtins.formatting.fish_indent,
        require("null-ls").builtins.diagnostics.markdownlint,
        -- require("null-ls").builtins.diagnostics.chktex,
        -- require("null-ls").builtins.diagnostics.selene,
    },
})
require("lspconfig")["null-ls"].setup({
    on_attach = custom_attach,
    capabilities = capabilities,
})
