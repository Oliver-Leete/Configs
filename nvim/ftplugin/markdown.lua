require("which-key").register({
    ["<localleader>"] = {
        h = { "<cmd>Telescope heading<cr>", "Headings" },
        p = { "<plug>MarkdownPreviewToggle", "Preview File" },
    },
}, {
    buffer = 0,
})

if vim.api.nvim_get_var("panelRepeat") == "q" then
    vim.api.nvim_set_var("panelRepeat", "o")
end
vim.api.nvim_buf_set_option(0, "textwidth", 100)

