require("nvim-treesitter.configs").setup({
    -- autopairs = { enable = true },
    indent = { enable = false },
    -- matchup = { enable = false },
    highlight = {
        enable = true,
        addditional_vim_regex_highlighting = false,
        disable = function(lang)
            return lang == "tex" or lang == "latex"
        end,
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

local tc_ex = function()
    tc.select_node(tc_settings)
    require("mini.operators").exchange("visual")
end

local tc_mul = function()
    tc.select_node(tc_settings)
    require("mini.operators").multipy("visual")
end

local tc_goto_first = function()
    repeat
        local old_pos = vim.api.nvim_win_get_cursor(0)
        tc.goto_prev(tc_settings)
        local new_pos = vim.api.nvim_win_get_cursor(0)
    until old_pos[1] == new_pos[1] and old_pos[2] == new_pos[2]
end

local tc_goto_last = function()
    repeat
        local old_pos = vim.api.nvim_win_get_cursor(0)
        tc.goto_next(tc_settings)
        local new_pos = vim.api.nvim_win_get_cursor(0)
    until old_pos[1] == new_pos[1] and old_pos[2] == new_pos[2]
end

local ts_surf_hint = [[
┏^^^^━━━━━┳━━━━━━┳━━━━━^^^^┓
┃^^^^     ┃ Tree ┃     ^^^^┃
┃^^^^     ┗━━━━━━┛     ^^^^┃
┃^^^^       Move       ^^^^┃
┣^^^^━━━━━━━━━━━━━━━━━━^^^^┫
┃ _h_/_j_/_k_/_l_: ←/↓/↑/→ ┃
┃^^     _[_/_]_: ⇚/⇛     ^^┃
┃^^^^                  ^^^^┃
┃^^   _H_/_L_: swap      ^^┃
┃^^^^                  ^^^^┃
┃^^   _R_: raise       ^^^^┃
┃^^   _m_: select node ^^^^┃
┣^^^^━━━━━━━━━━━━━━━━━━^^^^┫
┃^^    _<esc>_: exit   ^^^^┃
┗^^^^━━━━━━━━━━━━━━━━━━^^^^┛
]]
TreeHydra = require('hydra')({
    name = "Treesitter",
    mode = { "n", "x" },
    body = "Z",
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
        { '[',     tc_goto_first,                              { nowait = true } },
        { ']',     tc_goto_last,                               { nowait = true } },
        { 'h',     function() tc.goto_prev(tc_settings) end,   { nowait = true } },
        { 'j',     function() tc.goto_child(tc_settings) end,  { nowait = true } },
        { 'k',     function() tc.goto_parent(tc_settings) end, { nowait = true } },
        { 'l',     function() tc.goto_next(tc_settings) end,   { nowait = true } },
        { 'H',     function() tc.swap_prev(tc_settings) end,   { nowait = true } },
        { 'L',     function() tc.swap_next(tc_settings) end,   { nowait = true } },
        { 'R',     function() tc.raise(tc_settings) end,       { nowait = true } },
        { '$',     tc_ex,                                      { nowait = true, desc = false } },
        { '+',     tc_mul,                                     { nowait = true, desc = false } },
        { 'm',     function() tc.select_node(tc_settings) end, { exit = true } },
        { 'Z',     function() no_exit = true end,              { exit = true, nowait = true, desc = false } },
        { '<esc>', function() no_exit = true end,              { exit = true, nowait = true } },
    }
})

local tsj_utils = require("treesj.langs.utils")
local langs = require("treesj.langs")["presets"]

for _, item in pairs(langs.python) do
    if item.split then
        item.split.last_separator = true
    end
end

for _, item in pairs(langs) do
    item.comment = {
        both = {
            fallback = function()
                require("mini.splitjoin").toggle()
            end
        }
    }
end

langs.julia = {
    matrix_expression = { both = { separator = ';' }, join = { force_insert = ";" }, split = {} },
    argument_list = tsj_utils.set_preset_for_list({ join = { space_in_brackets = false } }),
    parameter_list = {
        join = { space_in_brackets = false },
        both = { last_separator = false, omit = { "keyword_parameters" } }
    },
    tuple_expression = tsj_utils.set_preset_for_list({ join = { space_in_brackets = false } }),
    parenthesized_expression = tsj_utils.set_preset_for_list({ join = { space_in_brackets = false } }),
    function_definition = {
        both = { omit = { "parameter_list" }, seperator = ";" },
        join = { force_insert = ";", space_in_brackets = true }
    },
    if_statement = { both = { seperator = ";" }, join = { force_insert = ";", space_in_brackets = true } },
    else_clause = { both = { seperator = ";" }, join = { force_insert = ";", space_in_brackets = true } },
    for_statement = {
        both = { omit = { "for_binding" }, seperator = ";" },
        join = { force_insert = ";", space_in_brackets = true }
    },
}

require('treesj').setup({
    use_default_keymaps = false,
    max_join_length = 1000,
    langs = langs,
})

vim.api.nvim_create_autocmd({ 'FileType' }, {
    pattern = '*',
    callback = function()
        local opts = { buffer = true }
        if langs[vim.bo.filetype] then
            vim.keymap.set("n", ",j", require("treesj").toggle, opts)
        else
            vim.keymap.set("n", ",j", require("mini.splitjoin").toggle, opts)
        end
    end
})
