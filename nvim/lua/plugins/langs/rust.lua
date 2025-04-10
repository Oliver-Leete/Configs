---@module "lazy"
---@type LazySpec
return {
    {
        "mrcjkb/rustaceanvim"
    },
    {
        'cordx56/rustowl',
        lazy = false,
        opts = {},
        keys = {
            { "<localleader>s", "<cmd>Rustowl toggle<cr>", desc = "Toggle Lifetimes", ft = "rust" },
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
