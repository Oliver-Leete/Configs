return {
    {
        "stsewd/sphinx.nvim",
        build = ":UpdateRemotePlugins"
    },
    {
        "neovim/nvim-lspconfig",
        ---@module "plugins.lsp"
        ---@type PluginLspOpts
        opts = {
            servers = {
                esbonio = {
                    mason = true,
                },
            },
        },
    },
}
