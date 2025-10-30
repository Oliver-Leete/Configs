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
        "akinsho/toggleterm.nvim",
        optional = true,
        opts = {
            ft_repls = {
                julia = {
                    cmd = { "julia", "--project=@." },
                },
            },
        },
    },
}
