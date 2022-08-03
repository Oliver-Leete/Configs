-- NOTE: If i move any of the treesitter stuff, remember to update the mini.nvim discussion comment
local parsers = require "nvim-treesitter.parsers"
local queries = require "nvim-treesitter.query"
local ts_utils = require "nvim-treesitter.ts_utils"
local textobject_at_point = function(query_list, pos, opts)
    opts = opts or {}
    local bufnr = vim.api.nvim_get_current_buf()
    local lang = parsers.get_buf_lang(bufnr)
    if not lang then
        return
    end

    local row, col = unpack(pos or vim.api.nvim_win_get_cursor(0))
    row = row - 1

    Query_list = query_list
    local matches = {}
    for _, query in pairs(query_list) do
        if not string.match(query, "^@.*") then
            error 'Captures must start with "@"'
        end
        vim.list_extend(matches, queries.get_capture_matches_recursively(bufnr, query, "textobjects"))
    end
    Matches = matches

    local match_length
    local smallest_range
    local earliest_start

    local lookahead_match_length
    local lookahead_largest_range
    local lookahead_earliest_start
    local lookbehind_match_length
    local lookbehind_largest_range
    local lookbehind_earliest_start

    for _, m in pairs(matches) do
        if opts.lookhere and m.node and ts_utils.is_in_node_range(m.node, row, col) then
            local length = ts_utils.node_length(m.node)
            if not match_length or length < match_length then
                smallest_range = m
                match_length = length
            end
            -- for nodes with same length take the one with earliest start
            if match_length and length == smallest_range then
                local start = m.start
                if start then
                    local _, _, start_byte = m.start.node:start()
                    if not earliest_start or start_byte < earliest_start then
                        smallest_range = m
                        match_length = length
                        earliest_start = start_byte
                    end
                end
            end
        elseif opts.lookahead then
            local start_line, start_col, start_byte = m.node:start()
            if start_line > row or start_line == row and start_col > col then
                local length = ts_utils.node_length(m.node)
                if not lookahead_earliest_start
                    or lookahead_earliest_start > start_byte
                    or (lookahead_earliest_start == start_byte and lookahead_match_length < length)
                then
                    lookahead_match_length = length
                    lookahead_largest_range = m
                    lookahead_earliest_start = start_byte
                end
            end
        elseif opts.lookbehind then
            local start_byte = m.node:start()
            local end_line, end_col = m.node:end_()
            if end_line < row or end_line == row and end_col < col then
                local length = ts_utils.node_length(m.node)
                if not lookbehind_earliest_start
                    or lookbehind_earliest_start < start_byte
                    or (lookbehind_earliest_start == start_byte and lookbehind_match_length > length)
                then
                    lookbehind_match_length = length
                    lookbehind_largest_range = m
                    lookbehind_earliest_start = start_byte
                end
            end
        end
    end
    if smallest_range then
        if smallest_range.start then
            local start_range = { smallest_range.start.node:range() }
            local node_range = { smallest_range.node:range() }
            return bufnr, { start_range[1], start_range[2], node_range[3], node_range[4] }, smallest_range.node
        else
            return bufnr, { smallest_range.node:range() }, smallest_range.node
        end
    elseif lookahead_largest_range then
        return bufnr, { lookahead_largest_range.node:range() }, lookahead_largest_range.node
    elseif lookbehind_largest_range then
        return bufnr, { lookbehind_largest_range.node:range() }, lookbehind_largest_range.node
    end
end

local miniAiTreesitter = function(ai_type, _, opts, query_list)
    -- NOTE: This function does not respect n_lines setting
    local new_opts = {}
    if opts.search_method:find("cover") then
        new_opts.lookhere = true
    end
    -- NOTE: nearest actually only looks back if nothing is found ahead
    if opts.search_method:find("nearest") then
        new_opts.lookahead = true
        new_opts.lookbehind = true
    elseif opts.search_method:find("next") then
        new_opts.lookahead = true
    elseif opts.search_method:find("prev") then
        new_opts.lookbehind = true
    end

    local full_query_list = {}
    for _, query in pairs(query_list) do
        if ai_type == "a" then
            table.insert(full_query_list, query .. ".outer")
        elseif ai_type == "i" then
            table.insert(full_query_list, query .. ".inner")
        end
    end

    local pos = vim.api.nvim_win_get_cursor(0)
    local count = opts.n_times
    local match_pos
    repeat
        count = count - 1
        local _, new_pos = textobject_at_point(full_query_list, pos, new_opts)
        if new_pos == nil and opts.search_method:find("nearest") then
            _, new_pos = textobject_at_point(full_query_list, pos, { lookbehind = true })
        end
        if new_pos == nil then
            break
        end
        match_pos = new_pos
        pos = { match_pos[3], match_pos[4] }
    until count == 0

    if not match_pos then return end
    return { from = { line = match_pos[1] + 1, col = match_pos[2] + 1 }, to = { line = match_pos[3] + 1,
        col = match_pos[4] } }
end

local miniAiTreeWrapper = function(query_list)
    if type(query_list) ~= "table" then
        query_list = {query_list}
    end
    return function(ai_type, _, opts)
        return miniAiTreesitter(ai_type, _, opts, query_list)
    end
end

local gen_spec = require('mini.ai').gen_spec
require("mini.ai").setup({
    custom_textobjects = {
        a = gen_spec.argument({ separators = { ',', ';' } }),
        o = miniAiTreeWrapper({"@block", "@conditional", "@loop"}),
        s = miniAiTreeWrapper({"@function", "@class"}),
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

Map({ "n", "x", "o" }, "]s", "<cmd>call v:lua.markAndGoMini(v:count, 'a', 'next', 's')<cr>")
Map({ "n", "x", "o" }, "[s", "<cmd>call v:lua.markAndGoMini(v:count, 'a', 'prev', 's')<cr>")

Map({ "n", "x", "o" }, "]o", "<cmd>call v:lua.markAndGoMini(v:count, 'a', 'next', 'o')<cr>")
Map({ "n", "x", "o" }, "[o", "<cmd>call v:lua.markAndGoMini(v:count, 'a', 'prev', 'o')<cr>")

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

-- removes the difference between inner and outer treesitter
-- not using mini, but related to surround
Map("n", "dpS", "misy<c-o>Ras", { remap = true })
Map("n", "dpO", "mioy<c-o>Rao", { remap = true })

require("mini.misc").setup({
    make_global = { "put_text", "zoom" },
})
