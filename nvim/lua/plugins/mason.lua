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
                "bibtex-tidy",
                "codelldb",
                "debugpy",
                "gitlint",
                "markdownlint",
                "rust-analyzer",
                "shellharden",
                "shfmt",
            },
        }
    },
}
