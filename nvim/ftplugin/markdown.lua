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
        ["["] = { "<cmd>let g:dirJumps='s'<cr>m`<plug>(textobj-markdown-Gheader-p)zz", "Header", noremap = false },
    },
    ["]"] = {
        s = { "<cmd>let g:dirJumps='s'<cr>m`<plug>(textobj-markdown-Gheader-n)zz", "Header", noremap = false },
        t = { "<cmd>let g:dirJumps='t'<cr>m`<plug>(textobj-markdown-header-p)zz", "Title", noremap = false },
        c = { "<cmd>let g:dirJumps='c'<cr>m`<plug>(textobj-markdown-chunk-n)zz", "Code Block", noremap = false },
        ["]"] = { "<cmd>let g:dirJumps='s'<cr>m`<plug>(textobj-markdown-Gheader-n)zz", "Header", noremap = false },
    },
}, {
    buffer = 0,
})
require("which-key").register({
    i = {
        n = {
            f = { ":<c-u>call v:lua.plug_targets(v:count, '(textobj-markdown-chunk-n)', '(textobj-markdown-chunk-i)')<cr>", "Code Fence" },
            t = { ":<c-u>call v:lua.plug_targets(v:count, '(textobj-markdown-text-n)', '(textobj-markdown-text-i)')<cr>", "Text" },
        },
        N = {
            f = { ":<c-u>call v:lua.plug_targets_back(v:count, '(textobj-markdown-chunk-p)', '(textobj-markdown-chunk-P)', '(textobj-markdown-chunk-i)')<cr>", "Code Fence" },
            t = { ":<c-u>call v:lua.plug_targets_back(v:count, '(textobj-markdown-text-p)', '(textobj-markdown-text-P)', '(textobj-markdown-text-i)')<cr>", "Text" },
        },
    },
    a = {
        n = {
            f = { ":<c-u>call v:lua.plug_targets(v:count, '(textobj-markdown-chunk-n)', '(textobj-markdown-chunk-a)')<cr>", "Code Fence" },
            t = { ":<c-u>call v:lua.plug_targets(v:count, '(textobj-markdown-text-n)', '(textobj-markdown-text-a)')<cr>", "Text" },
        },
        N = {
            f = { ":<c-u>call v:lua.plug_targets_back(v:count, '(textobj-markdown-chunk-p)', '(textobj-markdown-chunk-P)', '(textobj-markdown-chunk-a)')<cr>", "Code Fence" },
            t = { ":<c-u>call v:lua.plug_targets_back(v:count, '(textobj-markdown-text-p)', '(textobj-markdown-text-P)', '(textobj-markdown-text-a)')<cr>", "Text" },
        },
    },
}, {
    mode = "o",
    buffer = 0,
})
require("which-key").register({
    i = {
        n = {
            f = { ":<c-u>call v:lua.plug_targets(v:count, '(textobj-markdown-chunk-n)', '(textobj-markdown-chunk-i)')<cr>", "Code Fence" },
            t = { ":<c-u>call v:lua.plug_targets(v:count, '(textobj-markdown-text-n)', '(textobj-markdown-text-i)')<cr>", "Text" },
        },
        N = {
            f = { ":<c-u>call v:lua.plug_targets_back(v:count, '(textobj-markdown-chunk-p)', '(textobj-markdown-chunk-P)', '(textobj-markdown-chunk-i)')<cr>", "Code Fence" },
            t = { ":<c-u>call v:lua.plug_targets_back(v:count, '(textobj-markdown-text-p)', '(textobj-markdown-text-P)', '(textobj-markdown-text-i)')<cr>", "Text" },
        },
    },
    a = {
        n = {
            f = { ":<c-u>call v:lua.plug_targets(v:count, '(textobj-markdown-chunk-n)', '(textobj-markdown-chunk-a)')<cr>", "Code Fence" },
            t = { ":<c-u>call v:lua.plug_targets(v:count, '(textobj-markdown-text-n)', '(textobj-markdown-text-a)')<cr>", "Text" },
        },
        N = {
            f = { ":<c-u>call v:lua.plug_targets_back(v:count, '(textobj-markdown-chunk-p)', '(textobj-markdown-chunk-P)', '(textobj-markdown-chunk-a)')<cr>", "Code Fence" },
            t = { ":<c-u>call v:lua.plug_targets_back(v:count, '(textobj-markdown-text-p)', '(textobj-markdown-text-P)', '(textobj-markdown-text-a)')<cr>", "Text" },
        },
    },
}, {
    mode = "x",
    buffer = 0,
})

if vim.api.nvim_get_var("panelRepeat") == "q" then
    vim.api.nvim_set_var("panelRepeat", "o")
end
vim.api.nvim_buf_set_option(0, "textwidth", 100)

vim.fn["textobj#sentence#init"]()
