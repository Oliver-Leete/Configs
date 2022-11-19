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
        local to_col = #vim.api.nvim_buf_get_lines(0, to_line - 1, to_line, false)[1] + 1
        return {
            from = { line = from_line, col = from_col },
            to = { line = to_line, col = to_col },
        }
    end, hunks)

    return hunks
end

local gen_spec = require('mini.ai').gen_spec
local custom_objects = {
    -- argument
    a = gen_spec.argument({ separators = { ',', ';' } }),
    -- Brackets
    b = { { '%b()', '%b[]', '%b{}' }, '^.().*().$' },
    -- digits
    d = { '%f[%d]%d+' },
    -- diagnostics
    e = miniAiDiagnostics,
    -- Function call
    f = MiniAi.gen_spec.function_call(),
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
    -- key or value (needs a lot of work)
    -- k = gen_spec.argument({ brackets = { '%b()'}, separators = {',', ';', '=>'}}),
    -- blOck
    o = gen_spec.treesitter({
        a = { "@block.outer", "@conditional.outer", "@loop.outer" },
        i = { "@block.inner", "@conditional.inner", "@loop.inner" },
    }),
    -- paragraph
    p = { {
        '\n%s*\n()().-()\n%s*\n[%s]*()', -- normal paragraphs
        '^()().-()\n%s*\n[%s]*()', -- paragraph at start of file
        '\n%s*\n()().-()()$', -- paragraph at end of file
    } },
    -- Quotes
    q = { { "%b''", '%b""', '%b``' }, '^.().*().$' },
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
        a = { "@function.outer", "@class.outer", "@testitem.outer" },
        i = { "@function.inner", "@class.inner", "@testitem.inner" },
    }),
    -- Tag
    t = { '<(%w-)%f[^<%w][^<>]->.-</%1>', '^<.->().*()</[^/]->$' },
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
    -- chunk (as in from vim-textobj-chunk)
    z = {
        '\n.-%b{}',
        '\n().-%{\n().*()\n.*%}()'
    },
}

require("mini.ai").setup({
    custom_textobjects = custom_objects,

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

function _G.commandRepeat(leader, varName)
    local key = vim.api.nvim_get_var(varName)
    local return_map
    if key == "search" then
        if leader == "]" then
            return_map = "nvv"
        else
            return_map = "Nvv"
        end
    else
        return_map = leader .. key
    end
    return vim.api.nvim_replace_termcodes(return_map, true, true, true)
end

vim.g.dirJumps = "search"
Map({ "n", "x", "o" }, "n", "v:lua.commandRepeat(']', 'dirJumps')", { expr = true, remap = true })
Map({ "n", "x", "o" }, "N", "v:lua.commandRepeat('[', 'dirJumps')", { expr = true, remap = true })

Map({ "n", "x", "o" }, "[[", "[s", { remap = true })
Map({ "n", "x", "o" }, "]]", "]s", { remap = true })

Map({ "n", "x", "o" }, "[l", "<cmd>call v:lua.markAndGo(v:count, 'cprev', 'l')<cr>")
Map({ "n", "x", "o" }, "]l", "<cmd>call v:lua.markAndGo(v:count, 'cnext', 'l')<cr>")

for _, o in pairs(vim.tbl_keys(custom_objects)) do
    Map({ "n", "x", "o" }, "[" .. o, function() _G.markAndGoMini(vim.v.count, 'prev', o) end)
    Map({ "n", "x", "o" }, "]" .. o, function() _G.markAndGoMini(vim.v.count, 'next', o) end)
end

vim.api.nvim_create_autocmd("BufEnter",
    {
        group = vim.api.nvim_create_augroup("diff_mappings", { clear = true }),
        callback = function()
            local bmap = function(mode, key, action) Map(mode, key, action, { buffer = vim.api.nvim_get_current_buf() }) end
            if vim.wo.diff then
                bmap({ "n", "x", "o" }, "[h", "<cmd>call v:lua.markandGo(v:count, 'norm! [c', 'h')<cr>")
                bmap({ "n", "x", "o" }, "]h", "<cmd>call v:lua.markandGo(v:count, 'norm! ]c', 'h')<cr>")
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
