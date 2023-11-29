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

require("null-ls").register({
    name = "more_actions",
    method = { require("null-ls").methods.CODE_ACTION },
    filetypes = { "_all" },
    generator = {
        fn = require("ts-node-action").available_actions
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
