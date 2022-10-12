require("nvim-treesitter.configs").setup({
    -- autopairs = { enable = true },
    indent = { enable = false },
    matchup = { enable = true },
    highlight = {
        enable = true,
        addditional_vim_regex_highlighting = false,
        disable = function(lang)
            return lang == "tex" or lang == "latex"
        end,
    },
    playground = {
        enable = true,
        disable = {},
        updatetime = 25,
        persist_queries = false,
        keybindings = {
            toggle_query_editor = "o",
            toggle_hl_groups = "i",
            toggle_injected_languages = "t",
            toggle_anonymous_nodes = "a",
            toggle_language_display = "I",
            focus_language = "f",
            unfocus_language = "F",
            update = "R",
            goto_node = "<cr>",
            show_help = "?",
        },
    },
    query_linter = {
        enable = true,
        use_virtual_text = true,
        lint_events = { "BufWrite", "CursorHold" },
    },
})

require("iswap").setup({ keys = "tnseriaodhgjplfuwybkvmcxzq" })
require("trevj").setup({
    containers = {
        julia = {
            matrix_expression = { final_separator = ";", final_end_line = true, skip = { matrix_row = false } },
            tuple_expression = { final_separator = ",", final_end_line = true },
            argument_list = { final_separator = false, final_end_line = true },
            parameter_list = { final_separator = false, final_end_line = true },
        },
    },
})
