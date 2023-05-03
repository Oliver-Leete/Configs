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

require("ssr").setup({})

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

local tc_settings = {
    highlight = true,
    higroup = "Search",
}
local tc = require('tree-climber')

local no_exit = false
local ts_surf_hint = [[
┏^^^^━━━━━┳━━━━━━┳━━━━━^^^^┓
┃^^^^     ┃ Tree ┃     ^^^^┃
┃^^^^     ┗━━━━━━┛     ^^^^┃
┃^^^^       Move       ^^^^┃
┣^^^^━━━━━━━━━━━━━━━━━━^^^^┫
┃ _h_/_j_/_k_/_l_: ←/↓/↑/→ ┃
┃^^     _H_/_L_: ⇚/⇛     ^^┃
┃^^^^                  ^^^^┃
┃^^   _m_: select node ^^^^┃
┣^^^^━━━━━━━━━━━━━━━━━━^^^^┫
┃^^    _<esc>_: exit   ^^^^┃
┗^^^^━━━━━━━━━━━━━━━━━━^^^^┛
]]
require('hydra')({
    name = "Treesitter",
    mode = { "n", "x" },
    body = "S",
    config = {
        color = "red",
        invoke_on_body = true,
        hint = {
            position = "top-right",
            border = nil
        },
        on_enter = function() tc.highlight_node(tc_settings) end,
        on_exit = function()
            if no_exit == true then
                no_exit = false
            else
                tc.select_node(tc_settings)
            end
        end,
    },
    hint = ts_surf_hint,
    heads = {
        { 'h',     function() tc.goto_prev(tc_settings) end,   { nowait = true } },
        { 'j',     function() tc.goto_child(tc_settings) end,  { nowait = true } },
        { 'k',     function() tc.goto_parent(tc_settings) end, { nowait = true } },
        { 'l',     function() tc.goto_next(tc_settings) end,   { nowait = true } },
        { 'H',     function() tc.swap_prev(tc_settings) end,   { nowait = true } },
        { 'L',     function() tc.swap_next(tc_settings) end,   { nowait = true } },
        { 'm',     function() tc.select_node(tc_settings) end, { nowait = true } },
        { 'S',     function() no_exit = true end,              { exit = true, nowait = true, desc = false } },
        { '<esc>', function() no_exit = true end,              { exit = true, nowait = true } },
    }
})
