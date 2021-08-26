require("which-key").register({
    ["<leader>"] = {
        r = {},
    },
    ["<localleader>"] = {
        h = { "<cmd>Telescope heading<cr>", "Headings" },
        p = { "<plug>MarkdownPreviewToggle", "Preview File" },
    },
    ["["] = {
        s = { "<cmd>let g:dirJumps='s'<cr>m`<plug>(textobj-markdown-Gheader-p)zz", "Header", noremap = false },
        t = { "<cmd>let g:dirJumps='t'<cr>m`<plug>(textobj-markdown-header-p)zz", "Title", noremap = false },
        c = { "<cmd>let g:dirJumps='c'<cr>m`<plug>(textobj-markdown-chunk-p)zz", "Code Block", noremap = false },
    },
    ["]"] = {
        s = { "<cmd>let g:dirJumps='s'<cr>m`<plug>(textobj-markdown-Gheader-n)zz", "Header", noremap = false },
        t = { "<cmd>let g:dirJumps='t'<cr>m`<plug>(textobj-markdown-header-p)zz", "Title", noremap = false },
        c = { "<cmd>let g:dirJumps='c'<cr>m`<plug>(textobj-markdown-chunk-n)zz", "Code Block", noremap = false },
    },
}, {
    buffer = 0,
})
require("which-key").register({
    ["<leader>"] = {},
}, {
    mode = "x",
    buffer = 0,
})

if vim.api.nvim_get_var("dirJumps") == "f" then
    vim.api.nvim_set_var("dirJumps", "s")
end
if vim.api.nvim_get_var("panelRepeat") == "x" then
    vim.api.nvim_set_var("panelRepeat", "o")
end
