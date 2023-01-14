require("nvim-treesitter.configs").setup({
    -- autopairs = { enable = true },
    indent = { enable = false },
    matchup = { enable = false },
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

local tsj_utils = require('treesj.langs.utils')
require('treesj').setup({
    use_default_keymaps = false,
    max_join_length = 1000,

    langs = {
        julia = {
            matrix_expression = { both = { separator = ';' }, join = { force_insert = ";" }, split = {} },
            argument_list = tsj_utils.set_preset_for_list({ join = { space_in_brackets = false } }),
            parameter_list = { join = { space_in_brackets = false },
                both = { last_separator = false, omit = { "keyword_parameters" } } },
            tuple_expression = tsj_utils.set_preset_for_list({ join = { space_in_brackets = false } }),
            parenthesized_expression = tsj_utils.set_preset_for_list({ join = { space_in_brackets = false } }),
        },
    },
})

require("ts-node-action").setup({
    julia = require("ts-node-action.filetypes.julia")
})
