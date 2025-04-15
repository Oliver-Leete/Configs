---@module "lazy"
---@type LazySpec
return {
    {
        "folke/lazydev.nvim",
        ft = "lua",
        cmd = "LazyDev",
        opts = {
            library = {
                { path = "${3rd}/luv/library", words = { "vim%.uv" } },
                { path = "snacks.nvim", words = { "Snacks" } },
                { path = "nvim-chainsaw", words = { "Chainsaw" } },
            },
        },
    },
    {
        "saghen/blink.cmp",
        opts = {
            sources = {
                default = { "lazydev" },
                providers = {
                    lazydev = {
                        name = "LazyDev",
                        module = "lazydev.integrations.blink",
                        score_offset = 100,
                    },
                },
            },
        },
    },
    {
        "neovim/nvim-lspconfig",
        ---@module "plugins.lsp"
        ---@type PluginLspOpts
        opts = {
            servers = {
                lua_ls = {
                    mason = true,
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
                            hover = {
                                enumsLimit = 10,
                                enable = true,
                            },
                            hint = {
                                enable = true,
                                setType = true,
                                paramType = true,
                                paramName = "All",
                                semicolon = "SameLine",
                                arrayIndex = "Auto",
                            },
                            telemetry = {
                                enable = false,
                            },
                            format = {
                                enable = false,
                            },
                        },
                    },
                },
            },
        },
    },
}
