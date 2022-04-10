----------------------------------------------------------------------------------------------------
--                      _   _   ______    ____   __      __  _____   __  __                       --
--                     | \ | | |  ____|  / __ \  \ \    / / |_   _| |  \/  |                      --
--                     |  \| | | |__    | |  | |  \ \  / /    | |   | \  / |                      --
--                     | . ` | |  __|   | |  | |   \ \/ /     | |   | |\/| |                      --
--                     | |\  | | |____  | |__| |    \  /     _| |_  | |  | |                      --
--                     |_| \_| |______|  \____/      \/     |_____| |_|  |_|                      --
--                                                                                                --
----------------------------------------------------------------------------------------------------
-- Oliver Leete <oliverleete@gmail.com>                                                            --
-- https://github.com/oliver-leete                                                                 --
----------------------------------------------------------------------------------------------------

-- Treesitter

require("nvim-treesitter.configs").setup({
    autopairs = { enable = true },
    -- indent = { enable = true },
    highlight = {
        enable = true,
        addditional_vim_regex_highlighting = false
    },
    textobjects = {
        select = {
            enable = true,
            lookahead = true,
            keymaps = {},
        },
        move = {
            enable = true,
            set_jumps = true,
        },
        lsp_interop = {
            enable = true,
            border = "single",
            peek_definition_code = {
                ["<leader>kf"] = "@function.outer",
                ["<leader>ko"] = "@class.outer",
            },
        },
        swap = {
            enable = true,
        },
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
