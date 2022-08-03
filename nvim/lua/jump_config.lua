local function replace_keycodes(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

function _G.commandRepeat(leader, varName)
    local key = vim.api.nvim_get_var(varName)
    if key == "search" then
        if leader == "]" then
            return replace_keycodes("nvv")
        elseif leader == "[" then
            return replace_keycodes("Nvv")
        end
    end
    return replace_keycodes(leader .. key)
end

vim.g.dirJumps = "search"
Map({ "n", "x", "o" }, "n", "v:lua.commandRepeat(']', 'dirJumps')", { expr = true, remap = true })
Map({ "n", "x", "o" }, "N", "v:lua.commandRepeat('[', 'dirJumps')", { expr = true, remap = true })

Map({ "n", "x", "o" }, "[[", "[s", { remap = true })
Map({ "n", "x", "o" }, "]]", "]s", { remap = true })

Map({ "n", "x", "o" }, "[p", "<cmd>call v:lua.markGoCentre(v:count, 'norm! {', 'p')<cr>")
Map({ "x", "o" }, "alp", ":<c-u>call v:lua.paragraph_targets(v:count, 1, 1)<cr>")
Map({ "x", "o" }, "ilp", ":<c-u>call v:lua.paragraph_targets(v:count, 1, 1)<cr>")
Map({ "n", "x", "o" }, "]p", "<cmd>call v:lua.markGoCentre(v:count, 'norm! }', 'p')<cr>")
Map({ "x", "o" }, "anp", ":<c-u>call v:lua.paragraph_targets(v:count, 1)<cr>")
Map({ "x", "o" }, "inp", ":<c-u>call v:lua.paragraph_targets(v:count)<cr>")

Map({ "n", "x", "o"}, "[.", "<cmd>call v:lua.markAndGo(v:count, 'norm! (', '.')<cr>")
Map({ "n", "x", "o"}, "].", "<cmd>call v:lua.markAndGo(v:count, 'norm! )', '.')<cr>")

Map({ "n", "x", "o" }, "[e", [[<cmd>call v:lua.markGoCentre(v:count, 'lua vim.diagnostic.goto_prev({float={border="single",scope="cursor",source="always"}})', 'e')<cr>]])
Map({ "n", "x", "o" }, "]e", [[<cmd>call v:lua.markGoCentre(v:count, 'lua vim.diagnostic.goto_next({float={border="single",scope="cursor",source="always"}})', 'e')<cr>]])

Map({ "n", "x", "o" }, "[l", "<cmd>call v:lua.markGoCentre(v:count, 'try|cprevious|catch/E553/|clast|endtry', 'l')<cr>")
Map({ "n", "x", "o" }, "]l", "<cmd>call v:lua.markGoCentre(v:count, 'try|cnext|catch/E553/|cfirst|endtry', 'l')<cr>")

Map({ "n", "x", "o" }, "[w", "<cmd>call v:lua.markAndGo(v:count, 'norm! b', 'w')<cr>")
Map({ "x", "o" }, "alw", ":<c-u>call v:lua.mapped_targets(v:count, 'ge', 'aw')<cr>")
Map({ "x", "o" }, "ilw", ":<c-u>call v:lua.mapped_targets(v:count, 'ge', 'iw')<cr>")
Map({ "n", "x", "o" }, "]w", "<cmd>call v:lua.markAndGo(v:count, 'norm! w', 'w')<cr>")
Map({ "x", "o" }, "anw", ":<c-u>call v:lua.mapped_targets(v:count, 'w', 'aw')<cr>")
Map({ "x", "o" }, "inw", ":<c-u>call v:lua.mapped_targets(v:count, 'w', 'iw')<cr>")

Map({ "n", "x", "o" }, "[W", "<cmd>call v:lua.markAndGo(v:count, 'norm! B', 'W')<cr>")
Map({ "x", "o" }, "alW", ":<c-u>call v:lua.mapped_targets(v:count, 'gE', 'aW')<cr>")
Map({ "x", "o" }, "ilW", ":<c-u>call v:lua.mapped_targets(v:count, 'gE', 'iW')<cr>")
Map({ "n", "x", "o" }, "]W", "<cmd>call v:lua.markAndGo(v:count, 'norm! W', 'W')<cr>")
Map({ "x", "o" }, "anW", ":<c-u>call v:lua.mapped_targets(v:count, 'W', 'aW')<cr>")
Map({ "x", "o" }, "inW", ":<c-u>call v:lua.mapped_targets(v:count, 'W', 'iW')<cr>")


-- Map({"n", "x", "o"}, "[<m-w>", function() markAndGo("norm <m-b>)", "<m-w>")end)
-- Map({ "x", "o" }, "al<m-w>", ":<c-u>call v:lua.mapped_targets(v:count, '<m-ge>', 'a<m-w>')<cr>")
-- Map({ "x", "o" }, "il<m-w>", ":<c-u>call v:lua.mapped_targets(v:count, '<m-ge>', 'i<m-w>')<cr>")
-- Map({"n", "x", "o"}, "]<m-w>", function() markAndGo("norm <m-w>)", "<m-w>")end)
-- Map({ "x", "o" }, "an<m-w>", ":<c-u>call v:lua.mapped_targets(v:count, '<m-w>', 'a<m-w>')<cr>")
-- Map({ "x", "o" }, "in<m-w>", ":<c-u>call v:lua.mapped_targets(v:count, '<m-w>', 'i<m-w>')<cr>")


vim.api.nvim_create_autocmd("BufEnter",
    {
        group = vim.api.nvim_create_augroup("diff_mappings", { clear = true }),
        callback = function()
            local bmap = function(mode, key, action) Map(mode, key, action, { buffer = vim.api.nvim_get_current_buf() }) end
            if vim.wo.diff then
                bmap({ "n", "x", "o" }, "[h", "<cmd>call v:lua.markGoCentre(v:count, 'norm! [c', 'h')<cr>")
                bmap({ "n", "x", "o" }, "]h", "<cmd>call v:lua.markGoCentre(v:count, 'norm! ]c', 'h')<cr>")
            else
                bmap({ "x", "o" }, "ah", ":<c-u>Gitsigns selct_hunk<cr>")
                bmap({ "x", "o" }, "ih", ":<c-u>Gitsigns selct_hunk<cr>")
                bmap({ "n", "x", "o" }, "[h", [[<cmd>call v:lua.markGoCentre(v:count, 'lua require"gitsigns".prev_hunk()', 'h')<cr>]])
                bmap({ "x", "o" }, "alh", ":<c-u>call v:lua.git_target(v:count, 'false')<cr>")
                bmap({ "x", "o" }, "ilh", ":<c-u>call v:lua.git_target(v:count, 'false')<cr>")
                bmap({ "n", "x", "o" }, "]h", [[<cmd>call v:lua.markGoCentre(v:count, 'lua require"gitsigns".next_hunk()', 'h')<cr>]])
                bmap({ "x", "o" }, "anh", ":<c-u>call v:lua.git_target(v:count, 'true')<cr>")
                bmap({ "x", "o" }, "inh", ":<c-u>call v:lua.git_target(v:count, 'true')<cr>")
            end
        end
    }
)

-- Text object targets
function _G.git_target(count, go_forward)
    local move_cmd
    if go_forward == "true" then
        move_cmd = "Gitsigns next_hunk"
    else
        move_cmd = "Gitsigns prev_hunk"
    end
    repeat
        vim.cmd(move_cmd)
        count = count - 1
    until count <= 0
    vim.cmd("Gitsigns select_hunk")
end

function _G.mapped_targets(count, movement, selection)
    local cmd = ""
    repeat
        cmd = cmd .. movement
        count = count - 1
    until count <= 0
    cmd = cmd .. "m" .. selection
    vim.cmd([[normal ]] .. cmd)
end

function _G.plug_targets(count, movement, selection)
    local cmd = ""
    repeat
        cmd = cmd .. [[\<plug>]] .. movement
        count = count - 1
    until count <= 0

    cmd = cmd .. [[m\<plug>]] .. selection
    vim.cmd([[exe "normal ]] .. cmd .. [["]])
end

function _G.paragraph_end_jump(count)
    repeat
        vim.fn.search([[\v^$\n^\zs.+$]], "W")
        count = count - 1
    until count <= 0

    local line_diff = vim.fn.search([[\v(^.+$\n^$|\%$)]], "Wnc") - vim.fn.line(".")
    if line_diff > 0 then
        vim.cmd([[normal! V]] .. line_diff .. vim.api.nvim_eval('"j\\<esc>`>"'))
    else
        vim.cmd("normal! " .. vim.api.nvim_eval('"V\\<esc>`>"'))
    end
end

function _G.paragraph_end_jump_back(count)
    repeat
        vim.fn.search([[\v^.+\zs$\n^$]], "Wb")
        count = count - 1
    until count <= 0

    local line_diff = vim.fn.line(".") - vim.fn.search([[\v(^$\n^\zs.+$|\%^)]], "bWnc")
    if line_diff > 0 then
        vim.cmd([[normal! V]] .. line_diff .. vim.api.nvim_eval('"k\\<esc>`>"'))
    else
        vim.cmd("normal! " .. vim.api.nvim_eval('"V\\<esc>`>"'))
    end
end

function _G.paragraph_targets(count, around, back)
    local move_cmd
    if back then
        move_cmd = function() vim.fn.search([[\v^.+\zs$\n^$]], "Wb") end
    else
        move_cmd = function() vim.fn.search([[\v^$\n^\zs.+$]], "W") end
    end

    repeat
        move_cmd()
        count = count - 1
    until count <= 0

    if around then
        vim.cmd("norm! Vap")
    else
        vim.cmd("norm! Vip")
    end
end

function _G.markAndGo(count, command, key)
    vim.g.dirJumps = key
    vim.cmd("norm! m`")
    repeat
        vim.cmd(command)
        count = count - 1
    until count <= 0
end

function _G.markGoCentre(count, command, key)
    vim.g.dirJumps = key
    vim.cmd("norm! m`")
    repeat
        vim.cmd(command)
        count = count - 1
    until count <= 0
    vim.cmd("norm! zz")
end
