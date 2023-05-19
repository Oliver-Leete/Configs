require("neo-tree").setup({
    sources = {
        "filesystem",
        "buffers",
        "git_status",
        "diagnostics",
        "document_symbols",
    },
    source_selector = {
        winbar = true,
    },
})
