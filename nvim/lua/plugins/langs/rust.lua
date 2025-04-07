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
}
