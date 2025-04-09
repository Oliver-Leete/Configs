return {
    {
        "neovim/nvim-lspconfig",
        ---@module "plugins.lsp"
        ---@type PluginLspOpts
        opts = {
            servers = {
                fortls = {
                    mason = true,
                },
            },
        },
    },
}
