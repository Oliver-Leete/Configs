return {
    "stevearc/oil.nvim",
    opts = {
        keymaps = {
            ["<esc>"] = "actions.close",
        },
        delete_to_trash = true,
        lsp_file_methods = {
            timeout_ms = 2500,
        },
    },
    keys = {
        { "-", "<CMD>Oil<CR>", desc = "Open parent directory" }
    },
}
