return {
    {
        "neovim/nvim-lspconfig",
        ---@module "plugins.lsp"
        ---@type PluginLspOpts
        opts = {
            servers = {
                fish_lsp = {
                    mason = false,
                },
            },
        },
    },
}
