---@module "lazy"
---@type LazySpec
return {
    {
        "MrcJkb/haskell-tools.nvim",
        init = function()
            ---@module "haskell-tools"
            ---@type haskell-tools.Opts
            local opts = vim.defaulttable()
            opts.hls.settings.haskell.plugin.rename.config.crossModule = true
            opts.hls.settings.haskell.plugin.semanticTokens.globalOn = true

            vim.g.haskell_tools = opts
        end,
        lazy = false,
    },
    {
        "neovim/nvim-lspconfig",
        ---@module "plugins.lsp"
        ---@type PluginLspOpts
        opts = {
            setup = {
                hls = function() return true end,
            },
        },
    },
}
