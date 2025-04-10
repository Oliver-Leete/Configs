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
    bmap("n", "gd", function() require("snacks.picker").lsp_definitions() end, { desc = "Deffinition" })
    bmap("n", "gr", function() require("snacks.picker").lsp_references() end, { desc = "References" })
    bmap("n", "gD", function() require("snacks.picker").lsp_type_definitions() end, { desc = "Type Deffinition" })
    bmap("n", "gI", function() require("snacks.picker").lsp_implementations() end, { desc = "Implementations" })

    bmap("n", "go", function() require("trouble").toggle("lsp_outgoing_calls") end, { desc = "Outgoing Calls" })
    bmap("n", "gi", function() require("trouble").toggle("lsp_incoming_calls") end, { desc = "Incoming Calls" })
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

return {
    {
        "neovim/nvim-lspconfig",
        dependencies = {
            "mason.nvim",
            { "williamboman/mason-lspconfig.nvim", config = function() end },
        },
        opts = function()
            ---@class PluginLspOpts
            ---@field capabilities? lsp.ClientCapabilities
            ---@field servers? table<string, vim.lsp.ClientConfig>
            ---@field setup? table<string, fun(server: string, opts: vim.lsp.ClientConfig):boolean>
            local ret = {
                capabilities = {
                    workspace = {
                        fileOperations = {
                            didRename = true,
                            willRename = true,
                        },
                    },
                },
                servers = {},
                setup = {
                    -- return true if you don't want this server to be setup with lspconfig
                    -- example to setup with typescript.nvim
                    -- tsserver = function(_, opts)
                    --   require("typescript").setup({ server = opts })
                    --   return true
                    -- end,
                    -- Specify * to use this function as a fallback for any server
                    -- ["*"] = function(server, opts) end,
                },
            }
            return ret
        end,
        ---@param opts PluginLspOpts
        config = function(_, opts)
            vim.api.nvim_create_autocmd('LspAttach', {
                group = vim.api.nvim_create_augroup('UserLspConfig', {}),
                callback = function(ev)
                    local bufnr = ev.buf
                    local client = vim.lsp.get_client_by_id(ev.data.client_id)
                    custom_attach(client, bufnr)
                end,
            })

            local servers = opts.servers
            local has_cmp, cmp_nvim_lsp = pcall(require, "cmp_nvim_lsp")
            local has_blink, blink = pcall(require, "blink.cmp")
            local capabilities = vim.tbl_deep_extend(
                "force",
                {},
                vim.lsp.protocol.make_client_capabilities(),
                has_cmp and cmp_nvim_lsp.default_capabilities() or {},
                has_blink and blink.get_lsp_capabilities() or {},
                opts.capabilities or {}
            )

            local function setup(server)
                local server_opts = vim.tbl_deep_extend(
                    "force",
                    {capabilities = vim.deepcopy(capabilities),},
                    servers[server] or {}
                )
                if server_opts.enabled == false then
                    return
                end

                if opts.setup[server] then
                    if opts.setup[server](server, server_opts) then
                        return
                    end
                elseif opts.setup["*"] then
                    if opts.setup["*"](server, server_opts) then
                        return
                    end
                end
                require("lspconfig")[server].setup(server_opts)
            end

            -- get all the servers that are available through mason-lspconfig
            local have_mason, mlsp = pcall(require, "mason-lspconfig")
            local all_mslp_servers = {}
            if have_mason then
                all_mslp_servers = vim.tbl_keys(require("mason-lspconfig.mappings.server").lspconfig_to_package)
            end

            local ensure_installed = {} ---@type string[]
            for server, server_opts in pairs(servers) do
                if server_opts then
                    server_opts = server_opts == true and {} or server_opts
                    if server_opts.enabled ~= false then
                        -- run manual setup if mason=false or if this is a server that cannot be installed with mason-lspconfig
                        if server_opts.mason ~= true or not vim.tbl_contains(all_mslp_servers, server) then
                            setup(server)
                        else
                            ensure_installed[#ensure_installed + 1] = server
                        end
                    end
                end
            end

            if have_mason then
                mlsp.setup({
                    automatic_installation = false,
                    ensure_installed = vim.tbl_deep_extend("force", ensure_installed, {}),
                    handlers = { setup },
                })
            end
        end,
    }
}
