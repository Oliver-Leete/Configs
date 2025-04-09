return {
    {
        "neovim/nvim-lspconfig",
        ---@module "plugins.lsp"
        ---@type PluginLspOpts
        opts = {
            servers = {
                contextive = {
                    mason = true,
                },
            },
        },
    },
}
