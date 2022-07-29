require("mini.ai").setup({
    custom_textobjects = nil,

    mappings = {
        around = 'a',
        inside = 'i',

        goto_left = '{',
        goto_right = '}',
    },

    n_lines = 200,

    search_method = 'cover_or_nearest',
})

vim.g.miniindentscope_disable = true

require("mini.indentscope").setup({
    mappings = {
        object_scope = 'ii',
        object_scope_with_border = 'ai',
        goto_top = '{i',
        goto_bottom = '}i',
    },
    symbol = '╎',
})

require("mini.comment").setup({
    mappings = {
        comment = ',c',
        comment_line = ',cc',
        textobject = 'id',
    },
})

require("mini.pairs").setup({
    modes = { insert = true, command = true, terminal = true },
    mappings = {
        ['('] = { action = 'open', pair = '()', neigh_pattern = '[^\\].' },
        ['['] = { action = 'open', pair = '[]', neigh_pattern = '[^\\].' },
        ['{'] = { action = 'open', pair = '{}', neigh_pattern = '[^\\].' },

        [')'] = { action = 'close', pair = '()', neigh_pattern = '[^\\].' },
        [']'] = { action = 'close', pair = '[]', neigh_pattern = '[^\\].' },
        ['}'] = { action = 'close', pair = '{}', neigh_pattern = '[^\\].' },

        ['"'] = { action = 'closeopen', pair = '""', neigh_pattern = '[^\\].', register = { cr = false } },
        ["'"] = { action = 'closeopen', pair = "''", neigh_pattern = '[^%a\\].', register = { cr = false } },
        ['`'] = { action = 'closeopen', pair = '``', neigh_pattern = '[^\\].', register = { cr = false } },
    },
})

require("mini.surround").setup({
    mappings = {
        add = 'yp',
        delete = 'dp',
        find = 'fp',
        find_left = 'gP',
        replace = 'cp',
    },
    n_lines = 200,
    search_method = 'cover_or_nearest',
})

require("mini.misc").setup({
    make_global = { 'put', 'put_text' },
})
