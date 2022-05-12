-- TODO : Merge with in next/in last op bindings, have logic for n always following dirJumps
-- TODO : Make gn,gN bindings, I guess this is the same as the last note
-- TODO : Update filetype mappings
-- TODO : Make gn/gN repeat functions ignore capital letters in dirJumps variable

-- a = args
-- b = bracket FIX : Conflict with block
-- c = comment
-- d = diagnostic
-- e = end
-- E = END
-- $e = sub-word end
-- f = file
-- g = grammer (sentence)
-- h = hunk
-- i = ingot (code blocks)
-- j = ?
-- k = ?
-- l = list (quickfix)
-- m = ?
-- n = ? NOTE : might want to leave, as next
-- o = ?
-- p = paragraph
-- q = quote
-- r = ?
-- s = section
-- t = ?
-- u = ?
-- v = viewport
-- w = word
-- W = WORD
-- $w = sub-word
-- x = ?
-- y = ?
-- z = separators??
-- [] = also section
-- ,< = treesitter parameters TODO : move to a, once list item added to @parameter
-- / = search results

-- TODO : Integrate this stuff
--   local function gotoDiag(dir, sev)
--       return thunk(
--         vim.diagnostic["goto_" .. (dir == -1 and "prev" or "next")],
--         { enable_popup = true, severity = sev }
--       )
--     end
--     m.nname("<localleader>d", "LSP-Diagnostics")
--     nnoremap ([[<localleader>di]],                        vim.diagnostic.show,     "LSP: Show diagnostics")
--     nnoremap ({[[<localleader>dI]], [[<localleader>T]]},  require'trouble'.toggle, "LSP: Toggle Trouble")

--   -- print(7, vim.fn.reltimefloat(vim.fn.reltime()))
--     nnoremap ({[[<localleader>dd]], [[[d]]}, gotoDiag(-1),            "LSP: Goto prev diagnostic")
--     nnoremap ({[[<localleader>dD]], [[]d]]}, gotoDiag(1),             "LSP: Goto next diagnostic")
--     nnoremap ({[[<localleader>dw]], [[[w]]}, gotoDiag(-1, "Warning"), "LSP: Goto prev diagnostic (warning)")
--     nnoremap ({[[<localleader>dW]], [[]w]]}, gotoDiag(1,  "Warning"), "LSP: Goto next diagnostic (warning)")
--     nnoremap ({[[<localleader>de]], [[[e]]}, gotoDiag(-1, "Error"),   "LSP: Goto prev diagnostic (error)")
--     nnoremap ({[[<localleader>dE]], [[]e]]}, gotoDiag(1,  "Error"),   "LSP: Goto next diagnostic (error)")

vim.api.nvim_set_var("dirJumps", "search")
Map({"n", "x", "o"}, "n", "v:lua.commandRepeat(']', 'dirJumps')", {expr=true, remap=true})
Map({"n", "x", "o"}, "N", "v:lua.commandRepeat('[', 'dirJumps')", {expr=true, remap=true})

Map({"n", "x", "o"}, "[[", function() markGoCentre("TSTextobjectGotoPreviousStart @function.outer", "s")end)
Map({"n", "x", "o"}, "[]", function() markGoCentre("TSTextobjectGotoPreviousEnd @function.outer", "S")end)
Map({"n", "x", "o"}, "[s", function() markGoCentre("TSTextobjectGotoPreviousStart @function.outer", "s")end)
Map({"n", "x", "o"}, "[S", function() markGoCentre("TSTextobjectGotoPrevioutEnd @function.outer", "S")end)
Map({"n", "x", "o"}, "[,", function() markGoCentre("TSTextobjectGotoPreviousStart @parameter.inner", ",")end)
Map({"n", "x", "o"}, "[i", function() markGoCentre("TSTextobjectGotoPreviousStart @block.outer", "i")end)
Map({"n", "x", "o"}, "[<", function() markGoCentre("TSTextobjectGotoPreviousEnd @parameter.inner", "<")end)
Map({"n", "x", "o"}, "[I", function() markGoCentre("TSTextobjectGotoPreviousEnd @block.outer", "I")end)

Map({"n", "x", "o"}, "[h", function() markGoCentre("lua require'gitsigns'.prev_hunk()", "h")end)
Map({"n", "x", "o"}, "[l", function() markGoCentre("try|cprevious|catch/E553/|clast|endtry", "l")end)
Map({"n", "x", "o"}, "[/", function() markGoCentre("Nzz", "search")end)

Map({"n", "x", "o"}, "[p", function() markGoCentre("norm! {", "p")end)
Map({"n", "x", "o"}, "[P", function() markGoCentre("call v:lua.paragraph_end_jump_back(v:count)", "P")end)
Map({"n", "x", "o"}, "[w", function() markAndGo("norm! b", "w")end)
Map({"n", "x", "o"}, "[e", function() markAndGo("norm! ge", "e")end)
Map({"n", "x", "o"}, "[W", function() markAndGo("norm! B", "W")end)
Map({"n", "x", "o"}, "[E", function() markAndGo("norm! gE", "E")end)
Map({"n", "x", "o"}, "[$w", function() markAndGo("norm $b)", "$w")end)
Map({"n", "x", "o"}, "[$e", function() markAndGo("norm $ge)", "$e")end)

Map({"n", "x", "o"}, "[d", function() markGoCentre("lua vim.diagnostic.goto_prev({ float = {border='single', scope='cursor', source='always'}})", "e")end)

Map({"n", "x", "o"}, "]]", function() markGoCentre("TSTextobjectGotoNextStart @function.outer", "s")end)
Map({"n", "x", "o"}, "][", function() markGoCentre("TSTextobjectGotoNextEnd @function.outer", "S")end)
Map({"n", "x", "o"}, "]s", function() markGoCentre("TSTextobjectGotoNextStart @function.outer", "s")end)
Map({"n", "x", "o"}, "]S", function() markGoCentre("TSTextobjectGotoNextEnd @function.outer", "S")end)
Map({"n", "x", "o"}, "],", function() markGoCentre("TSTextobjectGotoNextStart @parameter.inner", ",")end)
Map({"n", "x", "o"}, "]<", function() markGoCentre("TSTextobjectGotoNextEnd @parameter.inner", "<")end)
Map({"n", "x", "o"}, "]i", function() markGoCentre("TSTextobjectGotoNextStart @block.outer", "i")end)
Map({"n", "x", "o"}, "]I", function() markGoCentre("TSTextobjectGotoNextEnd @block.outer", "I")end)

Map({"n", "x", "o"}, "]h", function() markGoCentre("lua require'gitsigns'.next_hunk()", "h")end)
Map({"n", "x", "o"}, "]l", function() markGoCentre("try|cnext|catch/E553/|cfirst|endtry", "l")end)
Map({"n", "x", "o"}, "]/", function() markGoCentre("nzz", "search")end)

Map({"n", "x", "o"}, "]p", function() markGoCentre("norm! }", "p")end)
Map({"n", "x", "o"}, "]P", function() markGoCentre("call v:lua.paragraph_end_jump(v:count)", "P")end)
Map({"n", "x", "o"}, "]w", function() markAndGo("norm! w", "w")end)
Map({"n", "x", "o"}, "]e", function() markAndGo("norm! e", "e")end)
Map({"n", "x", "o"}, "]W", function() markAndGo("norm! W", "W")end)
Map({"n", "x", "o"}, "]E", function() markAndGo("norm! E", "E")end)
Map({"n", "x", "o"}, "]$w", function() markAndGo("norm $w)", "$w")end)
Map({"n", "x", "o"}, "]$e", function() markAndGo("norm $e", "$e")end)

Map({"n", "x", "o"}, "]d", function() markGoCentre("lua vim.diagnostic.goto_next({float={border='single',scope='cursor',source='always'}})", "d")end)

if vim.api.nvim_win_get_option(0, "diff") then
    Map({"n", "x", "o"}, "[h", function() markGoCentre("norm! [c", "h")end)
    Map({"n", "x", "o"}, "]h", function() markGoCentre("norm! ]c", "h")end)
end

Map("o", "an", "v:lua.commandRepeat('a]', 'dirJumps')", {expr=true})
Map("o", "aN", "v:lua.commandRepeat('a[', 'dirJumps')", {expr=true})
Map("o", "in", "v:lua.commandRepeat('i]', 'dirJumps')", {expr=true})
Map("o", "iN", "v:lua.commandRepeat('i[', 'dirJumps')", {expr=true})
Map({"x", "o"}, "ah", ":<c-u>Gitsigns selct_hunk<cr>")
Map({"x", "o"}, "ai", ":<c-u>TSTextobjectSelect @block.outer<cr>")
Map({"x", "o"}, "a,", ":<c-u>TSTextobjectSelect @parameter.outer<cr>")
Map({"x", "o"}, "ac", ":<c-u>TSTextobjectSelect @comment.outer<cr>")
Map({"x", "o"}, "as", ":<c-u>TSTextobjectSelect @function.outer<cr>")
Map({"x", "o"}, "av", "<cmd>exec 'normal! HVL'<cr>")
Map({"x", "o"}, "af", "<cmd>exec 'normal! ggVG'<cr>")
    Map({"x", "o"}, "a]w", ":<c-u>call v:lua.mapped_targets(v:count, 'w', 'aw')<cr>")
    Map({"x", "o"}, "a]W", ":<c-u>call v:lua.mapped_targets(v:count, 'W', 'aW')<cr>")
    Map({"x", "o"}, "a]$w", ":<c-u>call v:lua.mapped_targets(v:count, '$w', 'a$w')<cr>")
    Map({"x", "o"}, "a]p", ":<c-u>call v:lua.paragraph_targets(v:count, 1)<cr>")
    Map({"x", "o"}, "a]h", ":<c-u>call v:lua.git_target(v:count, 'true')<cr>")
    Map({"x", "o"}, "a]i", ":<c-u>call v:lua.ts_target(v:count, '@block.outer')<cr>")
    Map({"x", "o"}, "a],", ":<c-u>call v:lua.ts_target(v:count, '@parameter.outer')<cr>")
    Map({"x", "o"}, "a]c", ":<c-u>call v:lua.ts_target(v:count, '@comment.outer')<cr>")
    Map({"x", "o"}, "a]s", ":<c-u>call v:lua.ts_target(v:count, '@function.outer')<cr>")
    Map({"x", "o"}, "a]o", ":<c-u>call v:lua.ts_target(v:count, '@class.outer')<cr>")
    Map({"x", "o"}, "a[w", ":<c-u>call v:lua.mapped_targets_back(v:count, 'b', 'ge', 'aw')<cr>")
    Map({"x", "o"}, "a[W", ":<c-u>call v:lua.mapped_targets_back(v:count, 'B', 'gE', 'aW')<cr>")
    Map({"x", "o"}, "a[$w", ":<c-u>call v:lua.mapped_targets_back(v:count, '$ge', '$b', 'a$w')<cr>")
    Map({"x", "o"}, "a[p", ":<c-u>call v:lua.paragraph_targets_back(v:count, 1)<cr>")
    Map({"x", "o"}, "a[h", ":<c-u>call v:lua.git_target(v:count, 'false')<cr>")
    Map({"x", "o"}, "a[i", ":<c-u>call v:lua.ts_target_back(v:count, '@block.outer')<cr>")
    Map({"x", "o"}, "a[,", ":<c-u>call v:lua.ts_target_back(v:count, '@parameter.outer')<cr>")
    Map({"x", "o"}, "a[c", ":<c-u>call v:lua.ts_target_back(v:count, '@comment.outer')<cr>")
    Map({"x", "o"}, "a[s", ":<c-u>call v:lua.ts_target_back(v:count, '@function.outer')<cr>")
Map({"x", "o"}, "ih", ":<c-u>Gitsigns selct_hunk<cr>")
Map({"x", "o"}, "ii", ":<c-u>TSTextobjectSelect @block.inner<cr>")
Map({"x", "o"}, "i,", ":<c-u>TSTextobjectSelect @parameter.inner<cr>")
Map({"x", "o"}, "ic", ":<c-u>TSTextobjectSelect @comment.outer<cr>")
Map({"x", "o"}, "is", ":<c-u>TSTextobjectSelect @function.inner<cr>")
Map({"x", "o"}, "iv", "<cmd>exec 'normal! HVL'<cr>")
Map({"x", "o"}, "if", "<cmd>exec 'normal! ggVG'<cr>")
    Map({"x", "o"}, "i]w", ":<c-u>call v:lua.mapped_targets(v:count, 'w', 'iw')<cr>")
    Map({"x", "o"}, "i]W", ":<c-u>call v:lua.mapped_targets(v:count, 'W', 'iW')<cr>")
    Map({"x", "o"}, "i]$w", ":<c-u>call v:lua.mapped_targets(v:count, '$w', 'i$w')<cr>")
    Map({"x", "o"}, "i]p", ":<c-u>call v:lua.paragraph_targets(v:count, 0)<cr>")
    Map({"x", "o"}, "i]h", ":<c-u>call v:lua.git_target(v:count, 'true')<cr>")
    Map({"x", "o"}, "i]i", ":<c-u>call v:lua.ts_target(v:count, '@block.inner')<cr>")
    Map({"x", "o"}, "i],", ":<c-u>call v:lua.ts_target(v:count, '@parameter.inner')<cr>")
    Map({"x", "o"}, "i]c", ":<c-u>call v:lua.ts_target(v:count, '@comment.outer')<cr>")
    Map({"x", "o"}, "i]s", ":<c-u>call v:lua.ts_target(v:count, '@function.inner')<cr>")
    Map({"x", "o"}, "i[w", ":<c-u>call v:lua.mapped_targets_back(v:count, 'b', 'ge', 'iw')<cr>")
    Map({"x", "o"}, "i[W", ":<c-u>call v:lua.mapped_targets_back(v:count, 'B', 'gE', 'iW')<cr>")
    Map({"x", "o"}, "i[$w", ":<c-u>call v:lua.mapped_targets_back(v:count, '$ge', '$b', 'i$w')<cr>")
    Map({"x", "o"}, "i[p", ":<c-u>call v:lua.paragraph_targets_back(v:count, 0)<cr>")
    Map({"x", "o"}, "i[h", ":<c-u>call v:lua.git_target(v:count, 'false')<cr>")
    Map({"x", "o"}, "i[i", ":<c-u>call v:lua.ts_target_back(v:count, '@block.inner')<cr>")
    Map({"x", "o"}, "i[,", ":<c-u>call v:lua.ts_target_back(v:count, '@parameter.inner')<cr>")
    Map({"x", "o"}, "i[c", ":<c-u>call v:lua.ts_target_back(v:count, '@comment.outer')<cr>")
    Map({"x", "o"}, "i[s", ":<c-u>call v:lua.ts_target_back(v:count, '@function.inner')<cr>")

-- Text object targets
function _G.ts_target(count, object)
    vim.cmd("TSTextobjectGotoNextStart " .. object)
    count = count - 1
    while count > 0 do
        vim.cmd("TSTextobjectGotoNextStart " .. object)
        count = count - 1
    end
    vim.cmd("TSTextobjectSelect " .. object)
end

function _G.ts_target_back(count, object)
    vim.cmd("TSTextobjectGotoPreviousEnd " .. object)
    count = count - 1
    while count > 0 do
        vim.cmd("TSTextobjectGotoPreviousEnd " .. object)
        count = count - 1
    end
    vim.cmd("TSTextobjectGotoPreviousStart " .. object)
    vim.cmd("TSTextobjectSelect " .. object)
end

function _G.git_target(count, go_forward)
    local move_cmd
    if go_forward == "true" then
        move_cmd = "Gitsigns next_hunk"
    else
        move_cmd = "Gitsigns prev_hunk"
    end
    vim.cmd(move_cmd)
    count = count - 1
    while count > 0 do
        vim.cmd(move_cmd)
        count = count - 1
    end
    vim.cmd("Gitsigns select_hunk")
end

function _G.mapped_targets(count, movement, selection)
    local cmd = movement
    count = count - 1
    while count > 0 do
        cmd = cmd .. movement
        count = count - 1
    end
    cmd = cmd .. "v" .. selection
    vim.cmd([[normal ]] .. cmd)
end

function _G.mapped_targets_back(count, movement, end_movement, selection)
    local cmd = end_movement
    count = count - 1
    while count > 0 do
        cmd = cmd .. movement
        count = count - 1
    end
    cmd = cmd .. "v" .. selection
    vim.cmd([[normal ]] .. cmd)
end

function _G.plug_targets(count, movement, selection)
    local cmd = [[\<plug>]] .. movement
    count = count - 1
    while count > 0 do
        cmd = cmd .. [[\<plug>]] .. movement
        count = count - 1
    end
    cmd = cmd .. [[v\<plug>]] .. selection
    vim.cmd([[exe "normal ]] .. cmd .. [["]])
end

function _G.plug_targets_back(count, movement, end_movement, selection)
    local cmd = [[\<plug>]] .. movement
    count = count - 1
    while count > 0 do
        cmd = cmd .. [[\<plug>]] .. movement
        count = count - 1
    end
    cmd = [[\<plug>]] .. end_movement
    cmd = cmd .. [[v\<plug>]] .. selection
    vim.cmd([[exe "normal ]] .. cmd .. [["]])
end

function _G.paragraph_end_jump(count)
    vim.fn.search([[\v^$\n^\zs.+$]], "W")
    count = count - 1
    while count > 0 do
        vim.fn.search([[\v^$\n^\zs.+$]], "W")
        count = count - 1
    end
    local line_diff = vim.fn.search([[\v(^.+$\n^$|\%$)]], "Wnc") - vim.fn.line(".")
    if line_diff > 0 then
        vim.cmd([[normal! V]] .. line_diff .. vim.api.nvim_eval('"j\\<esc>`>"'))
    else
        vim.cmd("normal! " .. vim.api.nvim_eval('"V\\<esc>`>"'))
    end
end

function _G.paragraph_end_jump_back(count)
    vim.fn.search([[\v^.+\zs$\n^$]], "Wb")
    count = count - 1
    while count > 0 do
        vim.fn.search([[\v^.+\zs$\n^$]], "Wb")
        count = count - 1
    end
    local line_diff = vim.fn.line(".") - vim.fn.search([[\v(^$\n^\zs.+$|\%^)]], "bWnc")
    if line_diff > 0 then
        vim.cmd([[normal! V]] .. line_diff .. vim.api.nvim_eval('"k\\<esc>`>"'))
    else
        vim.cmd("normal! " .. vim.api.nvim_eval('"V\\<esc>`>"'))
    end
end

function _G.paragraph_targets(count, around)
    vim.fn.search([[\v^$\n^\zs.+$]], "W")
    count = count - 1
    while count > 0 do
        vim.fn.search([[\v^$\n^\zs.+$]], "W")
        count = count - 1
    end
    if around == 1 then
        vim.cmd("norm! Vap")
    else
        vim.cmd("norm! Vip")
    end
end

function _G.paragraph_targets_back(count, around)
    vim.fn.search([[\v^.+\zs$\n^$]], "Wb")
    count = count - 1
    while count > 0 do
        vim.fn.search([[\v^.+\zs$\n^$]], "Wb")
        count = count - 1
    end
    if around == 1 then
        vim.cmd("norm! Vap")
    else
        vim.cmd("norm! Vip")
    end
end

function _G.markAndGo(command, key)
    vim.api.nvim_set_var("dirJumps", key)
    vim.cmd("norm! m`")
    vim.cmd(command)
end

function _G.markGoCentre(command, key)
    vim.api.nvim_set_var("dirJumps", key)
    vim.cmd("norm! m`")
    vim.cmd(command)
    vim.cmd("norm! zz")
end
