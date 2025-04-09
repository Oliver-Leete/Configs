local gen_spec = require("mini.ai").gen_spec

local gen_ai_spec = require('mini.extra').gen_ai_spec
local custom_objects = {
    -- Argument
    a = gen_spec.argument({ separator = "[,;]" }),
    -- Brackets
    b = { { "%b()", "%b[]", "%b{}" }, "^.().*().$" },
    -- Comments
    -- Digits
    d = gen_ai_spec.number(),
    -- diagnostics
    e = gen_ai_spec.diagnostic(),
    -- Function call
    f = gen_spec.function_call(),
    -- Grammer (sentence)
    g = {
        {
            "%b{}",
            "\n%s*\n()().-()\n%s*\n[%s]*()", -- normal paragraphs
            "^()().-()\n%s*\n[%s]*()",       -- paragraph at start of file
            "\n%s*\n()().-()()$",            -- paragraph at end of file
        },
        {
            "[%.?!][%s]+()().-[^%s].-()[%.?!]()[%s]",   -- normal sentence
            "^[%{%[]?[%s]*()().-[^%s].-()[%.?!]()[%s]", -- sentence at start of paragraph
            "[%.?!][%s]+()().-[^%s].-()()[\n%}%]]?$",   -- sentence at end of paragraph
            "^[%s]*()().-[^%s].-()()[%s]+$",            -- sentence at that fills paragraph (no final punctuation)
        }
    },
    -- Hunks
    -- Indents
    i = gen_ai_spec.indent(),
    -- Jumps
    -- key (from key value pair)
    k = gen_spec.treesitter({
        i = { "@assignment.lhs", "@key.inner" },
        a = { "@assignment.outer", "@key.inner" },
    }),
    -- List (quickfix)
    -- blOck
    o = gen_spec.treesitter({
        a = { "@block.outer", "@conditional.outer", "@loop.outer" },
        i = { "@block.inner", "@conditional.inner", "@loop.inner" },
    }),
    -- Paragraph
    p = { {
        "\n%s*\n()().-()\n%s*\n()[%s]*", -- normal paragraphs
        "^()().-()\n%s*\n[%s]*()",       -- paragraph at start of file
        "\n%s*\n()().-()()$",            -- paragraph at end of file
    } },
    -- Quotes
    q = { { "%b''", '%b""', "%b``" }, "^.().*().$" },
    -- sub-woRd (below w on my keyboard)
    r = {
        {
            "%u[%l%d]+%f[^%l%d]",
            "%f[%S][%l%d]+%f[^%l%d]",
            "%f[%P][%l%d]+%f[^%l%d]",
            "^[%l%d]+%f[^%l%d]",
        },
        "^().*()$"
    },
    -- Scope
    s = gen_spec.treesitter({
        a = { "@function.outer", "@class.outer", "@testitem.outer" },
        i = { "@function.inner", "@class.inner", "@testitem.inner" },
    }),
    -- Tag
    t = { "<(%w-)%f[^<%w][^<>]->.-</%1>", "^<.->().*()</[^/]->$" },
    -- Value (from key value pair)
    v = gen_spec.treesitter({
        i = { "@assignment.rhs", "@value.inner", "@return.inner" },
        a = { "@assignment.outer", "@value.inner", "@return.outer" },
    }),
    -- WORD
    W = { {
        "()()%f[%w%p][%w%p]+()[ \t]*()",
    } },
    -- word
    w = { "()()%f[%w_][%w_]+()[ \t]*()" },
    -- line (same key as visual line in my mappings)
    x = gen_ai_spec.line(),
    -- chunk (as in from vim-textobj-chunk)
    z = {
            "\n.-%b{}.-\n",
        "\n().-()%{\n.*\n.*%}().-\n()"
    },
    ["$"] = gen_spec.pair("$", "$", { type = "balanced" }),
}

require("mini.ai").setup({
    custom_textobjects = custom_objects,
    mappings = {
        around = "a",
        inside = "i",
        around_next = "an",
        inside_next = "in",
        around_last = "al",
        inside_last = "il",
        goto_left = "",
        goto_right = "",
    },
    n_lines = 500,
    search_method = "cover_or_nearest",
})

local mark_and_go_mini = function(direction, id, side)
    local count = vim.v.count
    vim.g.dirJumps = id
    id = id:lower()
    vim.cmd("norm! m`")
    repeat
        MiniAi.move_cursor(side, "a", id, { search_method = direction, n_times = vim.v.count })
        count = count - 1
    until count <= 0
end

local mark_and_select_mini = function(ai, id, direction)
    local count = vim.v.count
    vim.g.dirJumps = id
    id = id:lower()
    vim.cmd("norm! m`")
    repeat
        MiniAi.select_textobject(
            ai,
            id,
            {
                search_method = direction,
                vis_mode = "v"
            }
        )
        count = count - 1
    until count <= 0
end

local command_repeat = function(leader, varName)
    local key = vim.api.nvim_get_var(varName)
    local return_map
    local mode = vim.api.nvim_get_mode().mode
    if key == "search" then
        if leader == "]" then
            return_map = "n:norm! zz<cr>"
        else
            return_map = "N:norm! zz<cr>"
        end
    elseif mode == "v" or mode == "V" or mode == "" then
        if leader == "]" then
            return_map = "<esc>}" .. key
        else
            return_map = "<esc>{" .. key
        end
    else
        return_map = leader .. key
    end
    return vim.api.nvim_replace_termcodes(return_map, true, true, true)
end

vim.g.dirJumps = "search"
vim.keymap.set({ "n", "x", "o" }, "n", function() return command_repeat("]", "dirJumps") end, { expr = true, remap = true, silent = true })
vim.keymap.set({ "n", "x", "o" }, "N", function() return command_repeat("[", "dirJumps") end, { expr = true, remap = true, silent = true })

for _, o in pairs(vim.tbl_keys(custom_objects)) do
    vim.keymap.set({ "n", "x", "o" }, "[" .. o, function() mark_and_go_mini("prev", o, "left") end)
    vim.keymap.set({ "n", "x", "o" }, "]" .. o, function() mark_and_go_mini("next", o, "left") end)
    vim.keymap.set({ "n", "x", "o" }, "}" .. o, function() mark_and_select_mini("i", o, "next") end)
    vim.keymap.set({ "n", "x", "o" }, "{" .. o, function() mark_and_select_mini("i", o, "prev") end)

    vim.keymap.set({ "n", "x" }, "m" .. o,
        function() MiniAi.select_textobject("i", o, { search_method = "cover_or_nearest", vis_mode = "v" }) end)
    local O = o:upper()
    if O ~= o then
        vim.keymap.set({ "n", "x" }, "m" .. O,
            function() MiniAi.select_textobject("a", o, { search_method = "cover_or_nearest", vis_mode = "v" }) end)
        vim.keymap.set({ "n", "x", "o" }, "[" .. O, function() mark_and_go_mini("prev", O, "right") end)
        vim.keymap.set({ "n", "x", "o" }, "]" .. O, function() mark_and_go_mini("next", O, "right") end)
        vim.keymap.set({ "n", "x", "o" }, "}" .. O, function() mark_and_select_mini("a", O, "next") end)
        vim.keymap.set({ "n", "x", "o" }, "{" .. O, function() mark_and_select_mini("a", O, "prev") end)
    end
end

vim.api.nvim_create_autocmd("BufEnter",
    {
        group = vim.api.nvim_create_augroup("diff_mappings", { clear = true }),
        callback = function()
            local bmap = function(mode, key, action) vim.keymap.set(mode, key, action, { buffer = vim.api.nvim_get_current_buf() }) end
            if vim.wo.diff then
                bmap({ "n", "x", "o" }, "[h", "<cmd>call v:lua.markAndGo(v:count, 'norm! [c', 'h')<cr>")
                bmap({ "n", "x", "o" }, "]h", "<cmd>call v:lua.markAndGo(v:count, 'norm! ]c', 'h')<cr>")
            end
        end
    }
)

function _G.plug_targets(count, movement, selection)
    local cmd = ""
    repeat
        cmd = cmd .. [[\<plug>]] .. movement
        count = count - 1
    until count <= 0

    cmd = cmd .. [[m\<plug>]] .. selection
    vim.cmd([[exe "normal ]] .. cmd .. [["]])
end

function _G.markAndGo(count, command, key)
    vim.g.dirJumps = key
    vim.cmd("norm! m`")
    repeat
        vim.cmd(command)
        count = count - 1
    until count <= 0
end

local magmini = function(func, key, args, opts)
    local count = vim.v.count
    vim.g.dirJumps = key
    vim.cmd("norm! m`")
    repeat
        func(args, opts)
        count = count - 1
    until count <= 0
end

local bracketed = require("mini.bracketed")

vim.keymap.set({ "n", "x", "o" }, "[c", function() magmini(bracketed.comment, "c", "backward") end)
vim.keymap.set({ "n", "x", "o" }, "]c", function() magmini(bracketed.comment, "c", "forward") end)

vim.keymap.set({ "n", "x", "o" }, "[j", function() magmini(bracketed.jump, "j", "backward") end)
vim.keymap.set({ "n", "x", "o" }, "]j", function() magmini(bracketed.jump, "j", "forward") end)

vim.keymap.set({ "n" }, "[l", function() magmini(require("trouble").prev, "l", { jump = true, skip_groups = true, }) end, {desc = "trouble item"})
vim.keymap.set({ "n" }, "]l", function() magmini(require("trouble").next, "l", { jump = true, skip_groups = true, }) end, {desc = "trouble item"})

local diff = require("mini.diff")

vim.keymap.set({ "n", "x", "o" }, "[h", function() magmini(diff.goto_hunk, "h", "prev") end)
vim.keymap.set({ "n", "x", "o" }, "]h", function() magmini(diff.goto_hunk, "h", "next") end)

require('mini.bracketed').setup({
    buffer     = { suffix = "", options = {} },
    comment    = { suffix = "", options = {} },
    conflict   = { suffix = "", options = {} },
    diagnostic = { suffix = "", options = {} },
    file       = { suffix = "", options = {} },
    indent     = { suffix = "", options = {} },
    jump       = { suffix = "", options = {} },
    location   = { suffix = "", options = {} },
    oldfile    = { suffix = "", options = {} },
    quickfix   = { suffix = "", options = {} },
    treesitter = { suffix = "", options = {} },
    undo       = { suffix = "", options = {} },
    window     = { suffix = "", options = {} },
    yank       = { suffix = "y", options = {} },
})

local put_keys = { "p", "P" }
for _, lhs in ipairs(put_keys) do
    local rhs = 'v:lua.MiniBracketed.register_put_region("' .. lhs .. '")'
    vim.keymap.set({ "n", "x" }, lhs, rhs, { expr = true })
end

-- need to put this after targets
vim.keymap.set("x", "i", "<Plug>(niceblock-I)", { remap = true, nowait = true })
vim.keymap.set("x", "a", "<Plug>(niceblock-A)", { remap = true, nowait = true })
