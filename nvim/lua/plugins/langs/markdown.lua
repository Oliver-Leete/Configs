local ft = { "markdown", "quarto", "typst", "latex", "tex" }
---@module "lazy"
---@type LazySpec
return {
    {
        "OXY2DEV/markview.nvim",
        lazy = false,
        ---@module "markview"
        ---@type mkv.config
        opts = {
            preview = {
                enable = true,
                enable_hybrid_mode = true,
                modes = { "n", "no" },
                hybrid_modes = { "n", "no", "i", "s" },
                icon_provider = "mini",
            },
        },
        keys = {
            { "<localleader>p", "<cmd>Markview toggle<cr>",       desc = "Toggle preview",        ft = ft },
            { "<localleader>h", "<cmd>Markview hybridToggle<cr>", desc = "Toggle hybrid preview", ft = ft },
            { "<localleader>P", "<cmd>Markview Toggle<cr>",       desc = "Toggle all previews",   ft = ft },
            { "<localleader>o", "<cmd>Markview splitToggle<cr>",  desc = "Toggle split preview",  ft = ft },
            { "<localleader>O", "<cmd>Markview splitRedraw<cr>",  desc = "Redraw split preview",  ft = ft },
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
