return {
    'akinsho/toggleterm.nvim',
    dev = true,
    cmd = {
        "TermSelect",
        "TermExec",
        "TermNew",
        "ToggleTerm",
        "ToggleTermLast",
        "ToggleTermToggleAll",
        "ToggleTermSendVisualLines",
        "ToggleTermSendVisualSelection",
        "ToggleTermSendCurrentLine",
        "ToggleTermSetName",
    },
    opts = {
        sheml = "fish",
        shade_terminals = false,
    },
    keys = {
        { "<leader>tt", "<cmd>ToggleTermLast direction=horizontal<cr>", desc = "Toggle Terminal" },
        { "<leader>tn", "<cmd>TermNew<cr>",                             desc = "New Temminal" },
    }
}
