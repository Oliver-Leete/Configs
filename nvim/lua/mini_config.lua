local miniAiDiagnostics = function()
    local diagnostics = vim.diagnostic.get(0)
    diagnostics = vim.tbl_map(function(diagnostic)
        local from_line = diagnostic.lnum + 1
        local from_col = diagnostic.col + 1
        local to_line = diagnostic.end_lnum + 1
        local to_col = diagnostic.end_col + 1
        return {
            from = { line = from_line, col = from_col },
            to = { line = to_line, col = to_col }
        }
    end, diagnostics)

    return diagnostics
end

local miniAiGitsigns = function()
    local bufnr = vim.api.nvim_get_current_buf()
    local hunks = require("gitsigns.cache").cache[bufnr].hunks
    hunks = vim.tbl_map(function(hunk)
        local from_line = hunk.added.start
        local from_col = 1
        local to_line = hunk.vend
        local to_col = #vim.api.nvim_buf_get_lines(0, to_line-1, to_line, false)[1] + 1
        return {
            from = { line = from_line, col = from_col },
            to = { line = to_line, col = to_col },
        }
    end, hunks)

    return hunks
end

local gen_spec = require('mini.ai').gen_spec
require("mini.ai").setup({
    custom_textobjects = {
        -- argument
        a = gen_spec.argument({ separators = { ',', ';' } }),
        -- digits
        d = { '%f[%d]%d+' },
        -- diagnostics (errors)
        e = miniAiDiagnostics,
        -- grammer (sentence)
        g = {
            {
                '\n%s*\n()().-()\n%s*\n[%s]*()', -- normal paragraphs
                '^()().-()\n%s*\n[%s]*()', -- paragraph at start of file
                '\n%s*\n()().-()()$', -- paragraph at end of file
            },
            {
                '[%.?!][%s]+()().-[^%s].-()[%.?!]()[%s]', -- normal sentence
                '^[%s]*()().-[^%s].-()[%.?!]()[%s]', -- sentence at start of paragraph
                '[%.?!][%s]+()().-[^%s].-()()[\n]*$', -- sentence at end of paragraph
                '^[%s]*()().-[^%s].-()()[%s]+$', -- sentence at end of paragraph (no final punctuation)
            }
        },
        -- git hunks
        h = miniAiGitsigns,
        -- blOck
        o = gen_spec.treesitter({
            a = {"@block.outer", "@conditional.outer", "@loop.outer"},
            i = {"@block.inner", "@conditional.inner", "@loop.inner"},
        }),
        -- paragraph
        p = { {
            '\n%s*\n()().-()\n%s*\n[%s]*()', -- normal paragraphs
            '^()().-()\n%s*\n[%s]*()', -- paragraph at start of file
            '\n%s*\n()().-()()$', -- paragraph at end of file
        } },
        -- sub-word (below w on my keyboard)
        r = {
            {
                '%u[%l%d]+%f[^%l%d]',
                '%f[%S][%l%d]+%f[^%l%d]',
                '%f[%P][%l%d]+%f[^%l%d]',
                '^[%l%d]+%f[^%l%d]',
            },
            '^().*()$'
        },
        -- scope
        s = gen_spec.treesitter({
            a = {"@function.outer", "@class.outer"},
            i = {"@function.inner", "@class.inner"},
        }),
        -- line (same key as visual line in my mappings)
        x = { {
            '\n()%s*().-()\n()',
            '^()%s*().-()\n()'
        } },
        -- WORD
        W = { {
            '()()%f[%w%p][%w%p]+()[ \t]*()',
        } },
        -- word
        w = { '()()%f[%w_][%w_]+()[ \t]*()' },
        -- key or value (needs a lot of work)
        -- z = gen_spec.argument({ brackets = { '%b()'}, separators = {',', ';', '=>'}}),
        -- chunk (as in from vim-textobj-chunk)
        -- z = {
        --     '\n.-%b{}',
        --     '\n().-%{\n().*()\n.*%}()'
        -- },
    },

    mappings = {
        around = "a",
        inside = "i",

        around_next = 'an',
        inside_next = 'in',
        around_last = 'al',
        inside_last = 'il',

        goto_left = "",
        goto_right = "",
    },

    n_lines = 500,

    search_method = "cover_or_nearest",
})

function _G.markAndGoMini(count, direction, id)
    vim.g.dirJumps = id
    vim.cmd("norm! m`")
    repeat
        MiniAi.move_cursor("left", "a", id, { search_method = direction, n_times = vim.v.count })
        count = count - 1
    until count <= 0
end

for _, o in pairs({ "a", "b", "d", "e", "f", "g", "h", "o", "p", "q", "r", "s", "w", "W", "x", }) do
    Map({ "n", "x", "o" }, "[" .. o, "<cmd>call v:lua.markAndGoMini(v:count, 'prev', '" .. o .. "')<cr>")
    Map({ "n", "x", "o" }, "]" .. o, "<cmd>call v:lua.markAndGoMini(v:count, 'next', '" .. o .. "')<cr>")

    -- Does the same thing as goto_left and goto_right, but limited to the current object and for
    -- both inside and around. Now I don't need ninja-feet
    local m1 = "<cmd>lua MiniAi.move_cursor('"
    local m2 = "', '"
    local m3 = "', '" .. o .. "',  {n_times = vim.v.count, search_method='cover'} )<cr>"
    Map({ "n", "x", "o" }, "{" .. o, m1 .. "left" .. m2 .. "a" .. m3)
    Map({ "n", "x", "o" }, "}" .. o, m1 .. "right" .. m2 .. "a" .. m3)
    Map({ "n", "x", "o" }, "(" .. o, m1 .. "left" .. m2 .. "i" .. m3)
    Map({ "n", "x", "o" }, ")" .. o, m1 .. "right" .. m2 .. "i" .. m3)
end

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

-- removes the difference between inner and outer treesitter
-- not using mini, but related to surround
Map("n", "dpS", "misy<c-o>Ras", { remap = true })
Map("n", "dpO", "mioy<c-o>Rao", { remap = true })

require("mini.misc").setup({
    make_global = { "put_text", "zoom" },
})
