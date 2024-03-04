vim.g.miniindentscope_disable = true

require("mini.indentscope").setup({
    mappings = {
        object_scope = "",
        object_scope_with_border = "",
        goto_top = "",
        goto_bottom = "",

    },
    symbol = '▎"',
})

require("mini.comment").setup({
    mappings = {
        comment = ",c",
        comment_line = ",cc",
        textobject = "ic",
    },
})

require("mini.pairs").setup()

local ts_input = require("mini.surround").gen_spec.input.treesitter
require("mini.surround").setup({
    mappings = {
        add = "yp",
        visual_add = "P",
        delete = "dp",
        find = "gp",
        find_left = "gP",
        replace = "cp",
        update_n_lines = "",
        highlight = "",
    },
    n_lines = 200,
    search_method = "cover_or_nearest",
    custom_surroundings = {
        f = {
            input = ts_input({ outer = "@call.outer", inner = "@call.inner" })
        },
        o = {
            input = ts_input({
                outer = "@block.outer",
                inner = "@block.inner",
            }),
        },
        s = {
            input = ts_input({
                outer = "@function.outer",
                inner = "@function.inner",
            }),
        },
        k = {
            input = ts_input({
                outer = "@key.outer",
                inner = "@key.inner",
            }),
        },
        v = {
            input = ts_input({
                outer = "@value.outer",
                inner = "@value.inner",
            }),
        },
    }
})
vim.keymap.del("x", "yp")

require("mini.misc").setup({
    make_global = { "put_text", "zoom" },
})

require("mini.misc").setup_restore_cursor({
    center = true
})

require("mini.align").setup({
    mappings = {
        start = "",
        start_with_preview = ",t",
    }
})

local splitjoin = require("mini.splitjoin")
splitjoin.setup({
    mappings = {
        toggle = ",j",
    },
    detect = {
        separator = "[,;]",
    },
    split = {
        hooks_pre = {},
        hooks_post = {},
    },
    -- Join options
    join = {
        hooks_pre = {},
        hooks_post = {},
    },
})


local hipatterns = require("mini.hipatterns")
hipatterns.setup({
    highlighters = {
        hex_color = hipatterns.gen_highlighter.hex_color(),
    },
})

require("mini.files").setup({})


require("mini.operators").setup({
    evaluate = { prefix = ",=" },
    exchange = {
        prefix = "$",
        reindent_linewise = false,
    },
    multiply = {
        prefix = "+"
    },
    replace = {
        prefix = "R",
        reindent_linewise = false,
    },
    sort = {},
})

-- require("mini.notify").setup({
--     content = {
--         format = function(notif) return notif.msg end,
--     },
--     window = {
--         config = function()
--             local has_tabline = vim.o.showtabline == 2 or (vim.o.showtabline == 1 and #vim.api.nvim_list_tabpages() > 1)
--             local has_statusline = vim.o.laststatus > 0
--             local max_height = vim.o.lines - vim.o.cmdheight - (has_tabline and 1 or 0) - (has_statusline and 1 or 0)
--             local max_width = vim.o.columns
--             return {
--                 border = Border,
--                 relative = "editor",
--                 anchor = "SE",
--                 col = max_width,
--                 row = max_height,
--             }
--         end
--     }
-- })
-- vim.notify = require("mini.notify").make_notify()
