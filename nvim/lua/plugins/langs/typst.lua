---@module "lazy"
---@type LazySpec
return {
    {
        "neovim/nvim-lspconfig",
        ---@module "plugins.lsp"
        ---@type PluginLspOpts
        opts = {
            servers = {
                tinymist = {
                    mason = false,
                    settings = {
                        exportPdf = "onType",
                        outputPath = "$root/target/$dir/$name",
                    },
                },
            },
        },
    },
}
