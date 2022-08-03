local gen_spec = require('mini.ai').gen_spec
require("mini.ai").setup({
    custom_textobjects = {
        a = gen_spec.argument({ separators = { ',', ';' } }),
    },

    mappings = {
        around = "a",
        inside = "i",

        around_next = 'an',
        inside_next = 'in',
        around_last = 'al',
        inside_last = 'il',

        goto_left = "{",
        goto_right = "}",
    },

    n_lines = 200,

    search_method = "cover_or_nearest",
})

function _G.markAndGoMini(count, ai, np, key)
    vim.g.dirJumps = key
    vim.cmd("norm! m`")
    repeat
        MiniAi.move_cursor("left", ai, key, { search_method = np, n_times = vim.v.count })
        count = count - 1
    until count <= 0
end

Map({ "n", "x", "o" }, "]a", "<cmd>call v:lua.markAndGoMini(v:count, 'a', 'next', 'a')<cr>")
Map({ "n", "x", "o" }, "[a", "<cmd>call v:lua.markAndGoMini(v:count, 'a', 'prev', 'a')<cr>")

Map({ "n", "x", "o" }, "]b", "<cmd>call v:lua.markAndGoMini(v:count, 'a', 'next', 'b')<cr>")
Map({ "n", "x", "o" }, "[b", "<cmd>call v:lua.markAndGoMini(v:count, 'a', 'prev', 'b')<cr>")

Map({ "n", "x", "o" }, "]q", "<cmd>call v:lua.markAndGoMini(v:count, 'a', 'next', 'q')<cr>")
Map({ "n", "x", "o" }, "[q", "<cmd>call v:lua.markAndGoMini(v:count, 'a', 'prev', 'q')<cr>")

Map({ "n", "x", "o" }, "]f", "<cmd>call v:lua.markAndGoMini(v:count, 'a', 'next', 'f')<cr>")
Map({ "n", "x", "o" }, "[f", "<cmd>call v:lua.markAndGoMini(v:count, 'a', 'prev', 'f')<cr>")

vim.g.miniindentscope_disable = true

require("mini.indentscope").setup({
    mappings = {
        object_scope = "ii",
        object_scope_with_border = "ai",
        goto_top = "{i",
        goto_bottom = "}i",
    },
})

require("mini.comment").setup({
    mappings = {
        comment = ",c",
        comment_line = ",cc",
        textobject = "ic",
    },
})

require("mini.pairs").setup({
    modes = { insert = true, command = true, terminal = true },
    mappings = {
        ["("] = { action = "open", pair = "()", neigh_pattern = "[^\\][^%w]" },
        ["["] = { action = "open", pair = "[]", neigh_pattern = "[^\\][^%w]" },
        ["{"] = { action = "open", pair = "{}", neigh_pattern = "[^\\][^%w]" },

        [")"] = { action = "close", pair = "()", neigh_pattern = "[^\\]." },
        ["]"] = { action = "close", pair = "[]", neigh_pattern = "[^\\]." },
        ["}"] = { action = "close", pair = "{}", neigh_pattern = "[^\\]." },

        ['"'] = { action = "closeopen", pair = '""', neigh_pattern = "[^\\][^%w]", register = { cr = false } },
        ["'"] = { action = "closeopen", pair = "''", neigh_pattern = "[^%a\\][^%w]", register = { cr = false } },
        ["`"] = { action = "closeopen", pair = "``", neigh_pattern = "[^\\][^%w]", register = { cr = false } },
    },
})

require("mini.surround").setup({
    mappings = {
        add = "yp",
        visual_add = "P",
        delete = "dp",
        find = "fp",
        find_left = "gP",
        replace = "cp",
        update_n_lines = '',
    },
    n_lines = 200,
    search_method = "cover_or_nearest",
})
vim.keymap.del("x", "yp")

require("mini.misc").setup({
    make_global = { "put_text", "zoom" },
})
