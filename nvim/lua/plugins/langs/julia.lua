---@module "lazy"
---@type LazySpec
return {
    {
        "neovim/nvim-lspconfig",
        ---@module "plugins.lsp"
        ---@type PluginLspOpts
        opts = {
            servers = {
                julials = {
                    mason = true,
                },
            },
        },
    },
    {
        "kdheepak/nvim-dap-julia",
        ft = { "julia" },
        opts = {},
    },
    {
        "stevearc/overseer.nvim",
        ---@module "plugins.editor.overseer"
        ---@type OverseerUserConfig
        opts = {
            extra_templates = {
                julia = {
                    name = "Julia",
                    generator = function(_, cb)
                        cb({
                            {
                                name = "Run file",
                                builder = function()
                                    ---@type overseer.TaskDefinition
                                    return {
                                        name = "Running " .. vim.fn.expand("%:t:r"),
                                        cmd = { "julia", "--project=@.", vim.fn.expand("%:p") },
                                    }
                                end,
                                condition = {
                                    filetype = "julia",
                                },
                            }
                        })
                    end,
                },
            }
        }
    },
}
