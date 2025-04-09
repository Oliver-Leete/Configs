return {
    {
        "quarto-dev/quarto-nvim",
        dependencies = { "jmbuhr/otter.nvim", },
        opts = {
            codeRunner = {
                enabled = true,
                default_method = function()
                    
                end,
            },
        },
        keys = {
            { "<leader>qq", function() require("quarto").quartoPreview({}) end,      ft = "quarto", desc = "Open quarto preview" },
            { "<leader>qc", function() require("quarto").quartoClosePreview() end,   ft = "quarto", desc = "Close quarto preview" },
            { "<leader>qw", function() require("quarto").quartoPreviewNoWatch() end, ft = "quarto", desc = "Open quarto preview (no watch)" },
            { "<leader>qu", function() require("quarto").quartoUpdatePreview() end,  ft = "quarto", desc = "Update quarto preview" },
            { "<c-cr>",     function() require("quarto.runner").run_cell() end,      ft = "quarto", desc = "run cell" },
            { "<c-cr>",     function() require("quarto.runner").run_range() end,     ft = "quarto", desc = "run visual range" },
        }
    },
    {
        "jmbuhr/otter.nvim",
        opts = {
            lsp = {
                root_dir = function(_, bufnr)
                    return vim.fs.root(bufnr or 0, {
                        ".git",
                        "_quarto.yml",
                        "package.json",
                    }) or vim.fn.getcwd(0)
                end,
            },
        },
    },
}
