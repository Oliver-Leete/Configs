---@module "lazy"
---@type LazySpec
return {
    -- { "LhKipp/nvim-nu" },
    {
        "neovim/nvim-lspconfig",
        ---@module "plugins.lsp"
        ---@type PluginLspOpts
        opts = {
            servers = {
                nushell = {
                    mason = false,
                },
            },
        },
    },
}
