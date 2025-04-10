---@module "lazy"
---@type LazySpec
return {
    {
        "neovim/nvim-lspconfig",
        ---@module "plugins.lsp"
        ---@type PluginLspOpts
        opts = {
            servers = {
                ruff = {
                    mason = false,
                },
                basedpyright = {
                    mason = false,
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
        },
    },
    {
        "linux-cultist/venv-selector.nvim",
        dependencies = {
            "neovim/nvim-lspconfig",
            "mfussenegger/nvim-dap", "mfussenegger/nvim-dap-python", --optional
            { "nvim-telescope/telescope.nvim", branch = "0.1.x", dependencies = { "nvim-lua/plenary.nvim" } },
        },
        lazy = false,
        branch = "regexp",
        cmd = "VenvSelect",
        ft = "python",
        keys = {
            { "<localleader>v", "<cmd>VenvSelect<cr>" },
        },
        opts = {
            settings = {
                options = {
                    notify_user_on_venv_activation = true,
                },
            },
        },
    },
    {
        "mfussenegger/nvim-dap",
        dependencies = {
            {
                "mfussenegger/nvim-dap-python",
                config = function()
                    require("dap-python").setup("/home/oleete/.local/pipx/venvs/debugpy/bin/python")
                end,
                lazy = true,
            },
        },
    },
    {
        "Davidyz/coredumpy.nvim",
        cmd = { "Coredumpy" },
        opts = function()
            return { python = require("venv-selector").python, }
        end,
        dependencies = { "mfussenegger/nvim-dap" }
    },
    {
        "jay-babu/mason-nvim-dap.nvim",
        optional = true,
        ensure_installed = { "debugpy", },
        opts = {
            handlers = {
                -- Don't mess up DAP adapters provided by nvim-dap-python
                python = function() end,
            },
        },
    },
}
