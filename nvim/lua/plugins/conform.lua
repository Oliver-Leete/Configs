return {
    'stevearc/conform.nvim',
    config = function()
        require('conform').setup {
            formatters_by_ft = {
                quarto = { "injected" },
                julia = { "runic" },
                markdown = { "markdownlint" },
                lua = { lsp_format = "prefer" },
            },
        }
        require('conform').formatters.injected = {
            options = {
                lang_to_ext = {
                    bash = 'sh',
                    c_sharp = 'cs',
                    elixir = 'exs',
                    javascript = 'js',
                    julia = 'jl',
                    latex = 'tex',
                    markdown = 'md',
                    python = 'py',
                    ruby = 'rb',
                    rust = 'rs',
                    teal = 'tl',
                    r = 'r',
                    typescript = 'ts',
                },
                lang_to_formatters = {},
            },
        }
    end,
}
