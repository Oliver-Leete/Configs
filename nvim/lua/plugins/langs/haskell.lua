---@module "lazy"
---@type LazySpec
return {
    {
        "MrcJkb/haskell-tools.nvim",
        init = function()
            ---@module "haskell-tools"
            ---@type haskell-tools.Opts
            local opts = vim.defaulttable()
            opts.tools.repl.handler = "toggleterm"
            opts.hls.settings.haskell.plugin.rename.config.crossModule = true
            opts.hls.settings.haskell.plugin.semanticTokens.globalOn = true

            vim.g.haskell_tools = opts
        end,
        lazy = false,
    },
    {
        "kiyoon/haskell-scope-highlighting.nvim",
        ft = "haskell",
        keys = {
            {
                "<localleader>s",
                function()
                    local lsps = vim.lsp.get_clients({ bufnr = 0, name = "haskell_tools.nvim" })
                    if vim.b[0].haskell_scope_highlighting_on then
                        vim.b[0].haskell_scope_highlighting_on = false
                        vim.cmd.syntax("on")
                        vim.cmd.TSEnable("highlight")
                        for _, lsp in ipairs(lsps) do
                            vim.lsp.semantic_tokens.start(0, lsp.id)
                        end
                        vim.cmd.HaskellScopeHighlightDisable()
                    else
                        vim.b[0].haskell_scope_highlighting_on = true
                        vim.cmd.syntax("off")
                        vim.cmd.TSDisable("highlight")
                        for _, lsp in ipairs(lsps) do
                            vim.lsp.semantic_tokens.stop(0, lsp.id)
                        end
                        vim.cmd.HaskellScopeHighlightEnable()
                    end
                end,
                desc = "Toggle Scope Highlights",
                ft = "haskell",
            },
        },
        init = function()
            vim.api.nvim_create_autocmd("FileType", {
                pattern = "haskell",
                group = vim.api.nvim_create_augroup("haskell_toggle", {}),
                callback = function()
                    vim.cmd.HaskellScopeHighlightDisable()
                    vim.b[0].haskell_scope_highlighting_on = false
                end,
            })
        end,
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
