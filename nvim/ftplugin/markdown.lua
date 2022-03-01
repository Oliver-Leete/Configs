mapxName.group(mapxName.buffer, function()
nnoremap("<localleader>h",  "<cmd>Telescope heading<cr>", "Headings")
nnoremap("<localleader><localleader>",  "<plug>MarkdownPreviewToggle", "Preview File")
nnoremap("<localleader>,", function() require'telescope'.extensions.dict.synonyms() end, "Synonyms")
nnoremap(",fw", "m1!ippar w80<cr>`1", "Wrap Paragraph to Textwidth", silent)
end)

if vim.api.nvim_get_var("panelRepeat") == "q" then
    vim.api.nvim_set_var("panelRepeat", "o")
end
vim.api.nvim_buf_set_option(0, "textwidth", 80)
