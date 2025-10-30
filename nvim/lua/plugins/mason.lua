---@module "lazy"
---@type LazySpec
return {
    {
        "mason-org/mason.nvim",
        opts = {},
    },
    {
        "WhoIsSethDaniel/mason-tool-installer.nvim",
        dependencies = { "mason-org/mason.nvim" },
        opts = {
            ensure_installed = {
                "bibtex-tidy",
                "codelldb",
                "gitlint",
                "markdownlint",
                "rust-analyzer",
                "shellharden",
                "shfmt",
            },
        },
    },
}
