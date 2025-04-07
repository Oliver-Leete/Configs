return {
    "mfussenegger/nvim-lint",
    config = function()
        local markdownlint = require('lint').linters.markdownlint
        markdownlint.args = {
            "--stdin",
            "--disable",
            "MD013",
            "MD046",
            "MD009",
        }

        require("lint").linters_by_ft = {
            gitcommit = { "gitlint" },
            markdown = { "markdownlint" },
        }


        local lint_augroup = vim.api.nvim_create_augroup("lint_augroup", {})
        vim.api.nvim_create_autocmd("BufWritePost", {
            group = lint_augroup,
            callback = function()
                require("lint").try_lint()
                require("lint").try_lint("editorconfig-checker")
            end

        })
    end
}
