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

require("mini.pairs").setup({
    modes = { insert = true, command = true, terminal = true },
    mappings = {
        ["("] = { action = "open", pair = "()", neigh_pattern = "[^\\]%s" },
        ["["] = { action = "open", pair = "[]", neigh_pattern = "[^\\]%s" },
        ["{"] = { action = "open", pair = "{}", neigh_pattern = "[^\\]%s" },

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
        find = "gp",
        find_left = "gP",
        replace = "cp",
        update_n_lines = '',
    },
    n_lines = 200,
    search_method = "cover_or_nearest",
})
vim.keymap.del("x", "yp")

-- removes the difference between inner and outer treesitter
-- not using mini, but related to surround
Map("n", "dpS", "misy<c-o>Ras", { remap = true })
Map("n", "dpO", "mioy<c-o>Rao", { remap = true })

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
minimap.open()

Map("n", "<leader>:", minimap.toggle_focus)
Map("n", "<leader>;", minimap.toggle)
