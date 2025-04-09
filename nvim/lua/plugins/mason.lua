return {
    {
        "williamboman/mason.nvim",
        opts = {},
    },
    {
        "WhoIsSethDaniel/mason-tool-installer.nvim",
        dependencies = { "williamboman/mason.nvim", },
        opts = {
            ensure_installed = {
                "codelldb",
                "cpptools",
                "esbonio",
                "fortls",
                "gitlint",
                "json-lsp",
                "julia-lsp",
                "lua-language-server",
                "markdownlint",
                "marksman",
                "rust-analyzer",
                "shellharden",
                "shfmt",
                "taplo",
                "texlab",
                "yaml-language-server",
            },
        }
    },
}
