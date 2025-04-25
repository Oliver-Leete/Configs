---@module "lazy"
---@type LazySpec
return {
    {
        "mrcjkb/rustaceanvim",
    },
    {
        "cordx56/rustowl",
        lazy = false,
        opts = {},
        keys = {
            { "<localleader>s", "<cmd>Rustowl toggle<cr>", desc = "Toggle Lifetimes", ft = "rust" },
        },
    },
    {
        "neovim/nvim-lspconfig",
        ---@module "plugins.lsp"
        ---@type PluginLspOpts
        opts = {
            setup = {
                rust_analyzer = function() return true end,
            },
        },
    },
    {
        "Saecki/crates.nvim",
        event = { "BufRead Cargo.toml" },
        opts = {
            completion = {
                crates = {
                    enabled = true,
                },
            },
            lsp = {
                enabled = true,
                actions = true,
                completion = true,
                hover = true,
            },
        },
    },
}
