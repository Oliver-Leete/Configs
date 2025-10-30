local ft = { "markdown", "quarto", "typst", "latex", "tex", "yaml" }
---@module "lazy"
---@type LazySpec
return {
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
