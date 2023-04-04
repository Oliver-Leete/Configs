local gen_spec = require("mini.ai").gen_spec

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
        local to_col = #vim.api.nvim_buf_get_lines(bufnr, to_line - 1, to_line, false)[1] + 1
        return {
            from = { line = from_line, col = from_col },
            to = { line = to_line, col = to_col },
        }
    end, hunks)

    return hunks
end

local custom_objects = {
    -- argument
    a = gen_spec.argument({ separator = "[,;]" }),
    -- Brackets
    b = { { "%b()", "%b[]", "%b{}" }, "^.().*().$" },
    -- Comments
    -- digits
    d = { "%f[%d%._][%d%._]+" },
    -- diagnostics
    e = miniAiDiagnostics,
    -- Function call
    f = gen_spec.function_call(),
    -- grammer (sentence)
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
    -- git hunks
    h = miniAiGitsigns,
    -- Indents
    -- Jumps
    -- key (from key value pair)
    k = gen_spec.treesitter({
        i = { "@key.inner", "@assignment.lhs" },
        a = { "@key.inner", "@assignment.lhs" },
    }),
    -- List (quickfix)
    -- blOck
    o = gen_spec.treesitter({
        a = { "@block.outer", "@conditional.outer", "@loop.outer" },
        i = { "@block.inner", "@conditional.inner", "@loop.inner" },
    }),
    -- paragraph
    p = { {
        "\n%s*\n()().-()\n%s*\n[%s]*()", -- normal paragraphs
        "^()().-()\n%s*\n[%s]*()",       -- paragraph at start of file
        "\n%s*\n()().-()()$",            -- paragraph at end of file
    } },
    -- Quotes
    q = { { "%b''", '%b""', "%b``" }, "^.().*().$" },
    -- sub-word (below w on my keyboard)
    r = {
        {
            "%u[%l%d]+%f[^%l%d]",
            "%f[%S][%l%d]+%f[^%l%d]",
            "%f[%P][%l%d]+%f[^%l%d]",
            "^[%l%d]+%f[^%l%d]",
        },
        "^().*()$"
    },
    -- scope
    s = gen_spec.treesitter({
        a = { "@function.outer", "@class.outer", "@testitem.outer" },
        i = { "@function.inner", "@class.inner", "@testitem.inner" },
    }),
    -- Tag
    t = { "<(%w-)%f[^<%w][^<>]->.-</%1>", "^<.->().*()</[^/]->$" },
    -- value (from key value pair)
    v = gen_spec.treesitter({
        i = { "@value.inner", "@assignment.rhs" },
        a = { "@value.inner", "@assignment.rhs" },
    }),
    -- WORD
    W = { {
        "()()%f[%w%p][%w%p]+()[ \t]*()",
    } },
    -- word
    w = { "()()%f[%w_][%w_]+()[ \t]*()" },
    -- line (same key as visual line in my mappings)
    x = { {
        "\n()%s*().-()\n()",
        "^()%s*().-()\n()"
    } },
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
        goto_left = "{",
        goto_right = "}",
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

local command_repeat = function(leader, varName)
    local key = vim.api.nvim_get_var(varName)
    local return_map
    if key == "search" then
        if leader == "]" then
            return_map = "nzz"
        else
            return_map = "Nzz"
        end
    else
        return_map = leader .. key
    end
    return vim.api.nvim_replace_termcodes(return_map, true, true, true)
end

vim.g.dirJumps = "search"
Map({ "n", "x", "o" }, "n", function() return command_repeat("]", "dirJumps") end, { expr = true, remap = true })
Map({ "n", "x", "o" }, "N", function() return command_repeat("[", "dirJumps") end, { expr = true, remap = true })

Map({ "n", "x", "o" }, "[[", "[s", { remap = true })
Map({ "n", "x", "o" }, "]]", "]s", { remap = true })

for _, o in pairs(vim.tbl_keys(custom_objects)) do
    Map({ "n", "x", "o" }, "[" .. o, function() mark_and_go_mini("prev", o, "left") end)
    Map({ "n", "x", "o" }, "]" .. o, function() mark_and_go_mini("next", o, "left") end)
    local O = o:upper()
    if O ~= o then
        Map({ "n", "x", "o" }, "[" .. O, function() mark_and_go_mini("prev", O, "right") end)
        Map({ "n", "x", "o" }, "]" .. O, function() mark_and_go_mini("next", O, "right") end)
    end
end

vim.api.nvim_create_autocmd("BufEnter",
    {
        group = vim.api.nvim_create_augroup("diff_mappings", { clear = true }),
        callback = function()
            local bmap = function(mode, key, action) Map(mode, key, action, { buffer = vim.api.nvim_get_current_buf() }) end
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

Map({ "n", "x", "o" }, "[c", function() magmini(bracketed.comment, "c", "backward") end)
Map({ "n", "x", "o" }, "]c", function() magmini(bracketed.comment, "c", "forward") end)

Map({ "n", "x", "o" }, "[i", function() magmini(bracketed.indent, "i", "backward", { change_type = "diff" }) end)
Map({ "n", "x", "o" }, "]i", function() magmini(bracketed.indent, "i", "forward", { change_type = "diff" }) end)

Map({ "n", "x", "o" }, "[j", function() magmini(bracketed.jump, "j", "backward") end)
Map({ "n", "x", "o" }, "]j", function() magmini(bracketed.jump, "j", "forward") end)

Map({ "n", "x", "o" }, "[l", function() magmini(bracketed.quickfix, "l", "backward") end)
Map({ "n", "x", "o" }, "]l", function() magmini(bracketed.quickfix, "l", "forward") end)

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
