require("which-key").register({
    ["<localleader>"] = {
        h = {"<cmd>Telescope heading<cr>", "Headings"},
        p = {"<plug>MarkdownPreviewToggle", "Preview File"},
    }
}, {buffer=0})
