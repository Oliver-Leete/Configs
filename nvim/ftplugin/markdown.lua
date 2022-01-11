nnoremap("<localleader>h",  "<cmd>Telescope heading<cr>", "Headings")
nnoremap("<localleader>p",  "<plug>MarkdownPreviewToggle", "Preview File")
nnoremap("<localleader>,", function() require'telescope'.extensions.dict.synonyms() end, "Synonyms")

if vim.api.nvim_get_var("panelRepeat") == "q" then
    vim.api.nvim_set_var("panelRepeat", "o")
end
vim.api.nvim_buf_set_option(0, "textwidth", 100)

