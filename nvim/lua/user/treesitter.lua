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

local tsj_utils = require('treesj.langs.utils')
require('treesj').setup({
    use_default_keymaps = false,
    max_join_length = 1000,
    langs = {
        julia = {
            matrix_expression = { both = { separator = ';' }, join = { force_insert = ";" }, split = {} },
            argument_list = tsj_utils.set_preset_for_list({ join = { space_in_brackets = false } }),
            parameter_list = { join = { space_in_brackets = false }, both = { last_separator = false, omit = { "keyword_parameters" } } },
            tuple_expression = tsj_utils.set_preset_for_list({ join = { space_in_brackets = false } }),
            parenthesized_expression = tsj_utils.set_preset_for_list({ join = { space_in_brackets = false } }),
            function_definition = { both = { omit = { "parameter_list" }, seperator = ";" }, join = { force_insert = ";", space_in_brackets = true } },
            if_statement = { both = { seperator = ";" }, join = { force_insert = ";", space_in_brackets = true } },
            else_clause = { both = { seperator = ";" }, join = { force_insert = ";", space_in_brackets = true } },
            for_statement = { both = { omit = { "for_binding" }, seperator = ";" }, join = { force_insert = ";", space_in_brackets = true } },
        },
    },
})

require("ts-node-action").setup({
    julia = require("ts-node-action.filetypes.julia")
})

require "null-ls".register({
    name = "more_actions",
    method = { require "null-ls".methods.CODE_ACTION },
    filetypes = { "_all" },
    generator = {
        fn = require("ts-node-action").available_actions
    }
})

local ts_surf_settings = {
    highlight = true,
    higroup = "Search",
}
local ts_surf_hint = [[
┏^^^^━━━━━┳━━━━━━┳━━━━━^^^^┓
┃^^^^     ┃ Tree ┃     ^^^^┃
┃^^^^     ┗━━━━━━┛     ^^^^┃
┃^^^^       Move       ^^^^┃
┣^^^^━━━━━━━━━━━━━━━━━━^^^^┫
┃ _h_/_j_/_k_/_l_: ←/↓/↑/→ ┃
┃^^     _J_/_K_: ⟱/⤊     ^^┃
┃^^^^                  ^^^^┃
┃^^   _m_: select node ^^^^┃
┣^^^^━━━━━━━━━━━━━━━━━━^^^^┫
┃^^    _<esc>_: exit   ^^^^┃
┗^^^^━━━━━━━━━━━━━━━━━━^^^^┛
]]
require('hydra')({
    name = "Treesitter",
    mode = { "n", "x", "o" },
    body = "S",
    config = {
        color = "pink",
        invoke_on_body = true,
        hint = {
            position = "top-right",
            border = nil
        }
    },
    hint = ts_surf_hint,
    heads = {
        { 'h',     function() require('tree-climber').goto_parent(ts_surf_settings) end,    { nowait = true } },
        { 'j',     function() require('tree-climber').goto_next(ts_surf_settings) end,      { nowait = true } },
        { 'k',     function() require('tree-climber').goto_prev(ts_surf_settings) end,      { nowait = true } },
        { 'l',     function() require('tree-climber').goto_child(ts_surf_settings) end,     { nowait = true } },
        { 'K',     function() require('tree-climber').swap_prev(ts_surf_settings) end,      { nowait = true } },
        { 'J',     function() require('tree-climber').swap_next(ts_surf_settings) end,      { nowait = true } },
        { 'm',     function() require('tree-climber').select_node(ts_surf_settings) end,    { nowait = true } },
        { 'S',     function() require('tree-climber').highlight_node(ts_surf_settings) end, { exit = true, nowait = true, desc = false } },
        { '<esc>', function() require('tree-climber').highlight_node(ts_surf_settings) end, { exit = true, nowait = true } },
    }
})
