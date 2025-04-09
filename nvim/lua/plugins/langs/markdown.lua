return {
    {
        "OXY2DEV/markview.nvim",
        lazy = false,
        dependencies = {
            "saghen/blink.cmp"
        },
    },
    {
        "neovim/nvim-lspconfig",
        ---@module "plugins.lsp"
        ---@type PluginLspOpts
        opts = {
            servers = {
                marksman = {
                    mason = true,
                },
            },
        },
    },
}
