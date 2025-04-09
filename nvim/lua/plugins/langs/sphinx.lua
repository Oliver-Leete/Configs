return {
    {
        "stsewd/sphinx.nvim",
        build = ":UpdateRemotePlugins"
    },
    {
        "neovim/nvim-lspconfig",
        opts = { servers = { esbonio = {}, }, },
    },
}
