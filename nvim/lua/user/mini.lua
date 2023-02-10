vim.g.miniindentscope_disable = true

require("mini.indentscope").setup({
    mappings = {
        object_scope = "ii",
        object_scope_with_border = "ai",
        goto_top = "{i",
        goto_bottom = "}i",
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

local ts_input = require('mini.surround').gen_spec.input.treesitter
require("mini.surround").setup({
    mappings = {
        add = "yp",
        visual_add = "P",
        delete = "dp",
        find = "gp",
        find_left = "gP",
        replace = "cp",
        update_n_lines = '',
    },
    n_lines = 200,
    search_method = "cover_or_nearest",
    custom_surroundings = {
        f = {
            input = ts_input({ outer = '@call.outer', inner = '@call.inner' })
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

require("mini.align").setup({
    mappings = {
        start = "",
        start_with_preview = ",t",
    }
})

local perfanno_minimap = function()
    return function()
        local bufnr = vim.api.nvim_get_current_buf()
        local ret = require("perfanno.annotate").minimap[bufnr]
        if ret then
            return ret
        else
            return {}
        end
    end
end

local minimap = require("mini.map")
minimap.setup({
    integrations = {
        perfanno_minimap(),
        minimap.gen_integration.builtin_search(),
        minimap.gen_integration.diagnostic(),
        minimap.gen_integration.gitsigns(),
    },
    window = {
        show_integration_count = false,
    },
    symbols = {
        scroll_line = '🮚',
        scroll_view = '┃',
    },
})
-- minimap.open()

Map("n", "<leader>:", minimap.toggle_focus)
Map("n", "<leader>;", minimap.toggle)
