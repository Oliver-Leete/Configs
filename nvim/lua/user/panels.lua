require("trouble").setup({
    position = "right",
    action_keys = {
        jump_close = "<cr>",
        close = { "<esc>", "q" },
        toggle_preview = "p"
    },
    signs = {
        other = " "
    },
    auto_preview = false,
    use_diagnostic_signs = true,
    fold_closed = "",
    fold_open = "",
})
