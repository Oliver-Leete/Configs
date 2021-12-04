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

local expr = mapxName.expr

vim.api.nvim_set_var("dirJumps", "search")
nmap("n", "v:lua.commandRepeat(']', 'dirJumps')", expr)
xmap("n", "v:lua.commandRepeat(']', 'dirJumps')", expr)
omap("n", "v:lua.commandRepeat(']', 'dirJumps')", expr)
nmap("N", "v:lua.commandRepeat('[', 'dirJumps')", expr)
xmap("N", "v:lua.commandRepeat('[', 'dirJumps')", expr)
omap("N", "v:lua.commandRepeat('[', 'dirJumps')", expr)

mapxName.name("[", "Backward Leader")
nnoremap("[[", function() markGoCentre("TSTextobjectGotoPreviousStart @function.outer", "s")end, "Scope")
nnoremap("[]", function() markGoCentre("TSTextobjectGotoPreviousEnd @function.outer", "S")end, "Scope")
nnoremap("[s", function() markGoCentre("TSTextobjectGotoPreviousStart @function.outer", "s")end, "Scope")
nnoremap("[S", function() markGoCentre("TSTextobjectGotoPrevioutEnd @function.outer", "S")end, "Scope")
nnoremap("[,", function() markGoCentre("TSTextobjectGotoPreviousStart @parameter.inner", ",")end, "Parameter")
nnoremap("[i", function() markGoCentre("TSTextobjectGotoPreviousStart @block.outer", "i")end, "Block")
nnoremap("[<", function() markGoCentre("TSTextobjectGotoPreviousEnd @parameter.inner", "<")end, "Parameter")
nnoremap("[I", function() markGoCentre("TSTextobjectGotoPreviousEnd @block.outer", "I")end, "Block")
xnoremap("[[", function() markGoCentre("TSTextobjectGotoPreviousStart @function.outer", "s")end, "Scope")
xnoremap("[]", function() markGoCentre("TSTextobjectGotoPreviousEnd @function.outer", "S")end, "Scope")
xnoremap("[s", function() markGoCentre("TSTextobjectGotoPreviousStart @function.outer", "s")end, "Scope")
xnoremap("[S", function() markGoCentre("TSTextobjectGotoPrevioutEnd @function.outer", "S")end, "Scope")
xnoremap("[,", function() markGoCentre("TSTextobjectGotoPreviousStart @parameter.inner", ",")end, "Parameter")
xnoremap("[i", function() markGoCentre("TSTextobjectGotoPreviousStart @block.outer", "i")end, "Block")
xnoremap("[<", function() markGoCentre("TSTextobjectGotoPreviousEnd @parameter.inner", "<")end, "Parameter")
xnoremap("[I", function() markGoCentre("TSTextobjectGotoPreviousEnd @block.outer", "I")end, "Block")
onoremap("[[", function() markAndGo("TSTextobjectGotoPreviousStart @function.outer", "s")end, "Scope")
onoremap("[]", function() markAndGo("TSTextobjectGotoPreviousEnd @function.outer", "S")end, "Scope")
onoremap("[s", function() markAndGo("TSTextobjectGotoPreviousStart @function.outer", "s")end, "Scope")
onoremap("[S", function() markAndGo("TSTextobjectGotoPrevioutEnd @function.outer", "S")end, "Scope")
onoremap("[,", function() markAndGo("TSTextobjectGotoPreviousStart @parameter.inner", ",")end, "Parameter")
onoremap("[i", function() markAndGo("TSTextobjectGotoPreviousStart @block.outer", "i")end, "Block")
onoremap("[<", function() markAndGo("TSTextobjectGotoPreviousEnd @parameter.inner", "<")end, "Parameter")
onoremap("[I", function() markAndGo("TSTextobjectGotoPreviousEnd @block.outer", "I")end, "Block")

nnoremap("[h", function() markGoCentre("lua require'gitsigns'.prev_hunk()", "h")end, "Git Hunks")
nnoremap("[l", function() markGoCentre("try|cprevious|catch/E553/|clast|endtry", "l")end, "QuickFix Entry")
nnoremap("[/", function() markGoCentre("Nzz", "search")end, "Search Result")
xnoremap("[h", function() markGoCentre("lua require'gitsigns'.prev_hunk()", "h")end, "Git Hunks")
xnoremap("[l", function() markGoCentre("try|cprevious|catch/E553/|clast|endtry", "l")end, "QuickFix Entry")
xnoremap("[/", function() markGoCentre("Nzz", "search")end, "Search Result")
onoremap("[h", function() markAndGo("lua require'gitsigns'.prev_hunk()", "h")end, "Git Hunks")
onoremap("[l", function() markAndGo("try|cprevious|catch/E553/|clast|endtry", "l")end, "QuickFix Entry")
onoremap("[/", function() markAndGo("N", "search")end, "Search Result")

nnoremap("[p", function() markGoCentre("norm! {", "p")end, "Paragraph")
nnoremap("[P", function() markGoCentre("call v:lua.paragraph_end_jump_back(v:count)", "P")end, "Paragraph")
nnoremap("[w", function() markAndGo("norm! b", "w")end, "Word")
nnoremap("[e", function() markAndGo("norm! ge", "e")end, "Word End")
nnoremap("[W", function() markAndGo("norm! B", "W")end, "WORD")
nnoremap("[E", function() markAndGo("norm! gE", "E")end, "WORD End")
nnoremap("[$w", function() markAndGo("norm $b)", "$w")end, "Sub-Word")
nnoremap("[$e", function() markAndGo("norm $ge)", "$e")end, "Sub-Word End")
xnoremap("[p", function() markGoCentre("norm! {", "p")end, "Paragraph")
xnoremap("[P", function() markGoCentre("call v:lua.paragraph_end_jump_back(v:count)", "P")end, "Paragraph")
xnoremap("[w", function() markAndGo("norm! b", "w")end, "Word")
xnoremap("[e", function() markAndGo("norm! ge", "e")end, "Word End")
xnoremap("[W", function() markAndGo("norm! B", "W")end, "WORD")
xnoremap("[E", function() markAndGo("norm! gE", "E")end, "WORD End")
xnoremap("[$w", function() markAndGo("norm $b)", "$w")end, "Sub-Word")
xnoremap("[$e", function() markAndGo("norm $ge)", "$e")end, "Sub-Word End")
onoremap("[p", function() markAndGo("norm! {", "p")end, "Paragraph")
onoremap("[P", function() markGoCentre("call v:lua.paragraph_end_jump_back(v:count)", "P")end, "Paragraph")
onoremap("[w", function() markAndGo("norm! b", "w")end, "Word")
onoremap("[e", function() markAndGo("norm! ge", "e")end, "Word End")
onoremap("[W", function() markAndGo("norm! B", "W")end, "WORD")
onoremap("[E", function() markAndGo("norm! gE", "E")end, "WORD End")
onoremap("[$w", function() markAndGo("norm $b", "$w")end, "Sub-Word")
onoremap("[$e", function() markAndGo("norm $ge", "$e")end, "Sub-Word End")

nnoremap("[d", function() markGoCentre("lua vim.diagnostic.goto_prev({ float = {border='single', scope='cursor', source='always'}})", "e")end, "Diagnostics")
    mapxName.name("[D", "Diagnostaic Type")
    nnoremap("[Dn", function() markGoCentre("lua vim.diagnostic.goto_prev({severity='Error',float={border='single',scope='cursor',source='always'}})", "Da")end, "Error")
    nnoremap("[De", function() markGoCentre("lua vim.diagnostic.goto_prev({severity='Warn',float={border='single',scope='cursor',source='always'}})", "Dr")end, "Warn")
    nnoremap("[Di", function() markGoCentre("lua vim.diagnostic.goto_prev({severity='Info',float={border='single',scope='cursor',source='always'}})", "Ds")end, "Info")
    nnoremap("[Do", function() markGoCentre("lua vim.diagnostic.goto_prev({severity='Hint',float={border='single',scope='cursor',source='always'}})", "Dt")end, "Hint")
xnoremap("[d", function() markGoCentre("lua vim.diagnostic.goto_prev({ float = {border='single', scope='cursor', source='always'}})", "e")end, "Diagnostics")
    xnoremap("[Dn", function() markGoCentre("lua vim.diagnostic.goto_prev({severity='Error',float={border='single',scope='cursor',source='always'}})", "Da")end, "Error")
    xnoremap("[De", function() markGoCentre("lua vim.diagnostic.goto_prev({severity='Warn',float={border='single',scope='cursor',source='always'}})", "Dr")end, "Warn")
    xnoremap("[Di", function() markGoCentre("lua vim.diagnostic.goto_prev({severity='Info',float={border='single',scope='cursor',source='always'}})", "Ds")end, "Info")
    xnoremap("[Do", function() markGoCentre("lua vim.diagnostic.goto_prev({severity='Hint',float={border='single',scope='cursor',source='always'}})", "Dt")end, "Hint")
onoremap("[d", function() markAndGo("lua vim.diagnostic.goto_prev({ float = {border='single', scope='cursor', source='always'}})", "e")end, "Diagnostics")
    onoremap("[Dn", function() markAndGo("lua vim.diagnostic.goto_prev({severity='Error',float={border='single',scope='cursor',source='always'}})", "Da")end, "Error")
    onoremap("[De", function() markAndGo("lua vim.diagnostic.goto_prev({severity='Warn',float={border='single',scope='cursor',source='always'}})", "Dr")end, "Warn")
    onoremap("[Di", function() markAndGo("lua vim.diagnostic.goto_prev({severity='Info',float={border='single',scope='cursor',source='always'}})", "Ds")end, "Info")
    onoremap("[Do", function() markAndGo("lua vim.diagnostic.goto_prev({severity='Hint',float={border='single',scope='cursor',source='always'}})", "Dt")end, "Hint")

mapxName.name("]", "Forward Leader")
nnoremap("]]", function() markGoCentre("TSTextobjectGotoNextStart @function.outer", "s")end, "Scope")
nnoremap("][", function() markGoCentre("TSTextobjectGotoNextEnd @function.outer", "S")end, "Scope")
nnoremap("]s", function() markGoCentre("TSTextobjectGotoNextStart @function.outer", "s")end, "Scope")
nnoremap("]S", function() markGoCentre("TSTextobjectGotoNextEnd @function.outer", "S")end, "Scope")
nnoremap("],", function() markGoCentre("TSTextobjectGotoNextStart @parameter.inner", ",")end, "Parameter")
nnoremap("]<", function() markGoCentre("TSTextobjectGotoNextEnd @parameter.inner", "<")end, "Parameter (end)")
nnoremap("]i", function() markGoCentre("TSTextobjectGotoNextStart @block.outer", "i")end, "Block")
nnoremap("]I", function() markGoCentre("TSTextobjectGotoNextEnd @block.outer", "I")end, "Block (end)")
xnoremap("]]", function() markGoCentre("TSTextobjectGotoNextStart @function.outer", "s")end, "Scope")
xnoremap("][", function() markGoCentre("TSTextobjectGotoNextEnd @function.outer", "S")end, "Scope")
xnoremap("]s", function() markGoCentre("TSTextobjectGotoNextStart @function.outer", "s")end, "Scope")
xnoremap("]S", function() markGoCentre("TSTextobjectGotoNextEnd @function.outer", "S")end, "Scope")
xnoremap("],", function() markGoCentre("TSTextobjectGotoNextStart @parameter.inner", ",")end, "Parameter")
xnoremap("]<", function() markGoCentre("TSTextobjectGotoNextEnd @parameter.inner", "<")end, "Parameter (end)")
xnoremap("]i", function() markGoCentre("TSTextobjectGotoNextStart @block.outer", "i")end, "Block")
xnoremap("]I", function() markGoCentre("TSTextobjectGotoNextEnd @block.outer", "I")end, "Block (end)")
onoremap("]]", function() markAndGo("TSTextobjectGotoNextStart @function.outer", "s")end, "Scope")
onoremap("][", function() markAndGo("TSTextobjectGotoNextEnd @function.outer", "S")end, "Scope")
onoremap("]s", function() markAndGo("TSTextobjectGotoNextStart @function.outer", "s")end, "Scope")
onoremap("]S", function() markAndGo("TSTextobjectGotoNextEnd @function.outer", "S")end, "Scope")
onoremap("],", function() markAndGo("TSTextobjectGotoNextStart @parameter.inner", ",")end, "Parameter")
onoremap("]<", function() markAndGo("TSTextobjectGotoNextEnd @parameter.inner", "<")end, "Parameter (end)")
onoremap("]i", function() markAndGo("TSTextobjectGotoNextStart @block.outer", "i")end, "Block")
onoremap("]I", function() markAndGo("TSTextobjectGotoNextEnd @block.outer", "I")end, "Block (end)")

nnoremap("]h", function() markGoCentre("lua require'gitsigns'.next_hunk()", "h")end, "Git Hunks")
nnoremap("]l", function() markGoCentre("try|cnext|catch/E553/|cfirst|endtry", "l")end, "QuickFix Entry")
nnoremap("]/", function() markGoCentre("nzz", "search")end, "Search Result")
xnoremap("]h", function() markGoCentre("lua require'gitsigns'.next_hunk()", "h")end, "Git Hunks")
xnoremap("]l", function() markGoCentre("try|cnext|catch/E553/|cfirst|endtry", "l")end, "QuickFix Entry")
xnoremap("]/", function() markGoCentre("nzz", "search")end, "Search Result")
onoremap("]h", function() markAndGo("lua require'gitsigns'.next_hunk()", "h")end, "Git Hunks")
onoremap("]l", function() markAndGo("try|cnext|catch/E553/|cfirst|endtry", "l")end, "QuickFix Entry")
onoremap("]/", function() markAndGo("n", "search")end, "Search Result")

nnoremap("]p", function() markGoCentre("norm! }", "p")end, "Paragraph")
nnoremap("]P", function() markGoCentre("call v:lua.paragraph_end_jump(v:count)", "P")end, "Paragraph")
nnoremap("]w", function() markAndGo("norm! w", "w")end, "Word")
nnoremap("]e", function() markAndGo("norm! e", "e")end, "Word")
nnoremap("]W", function() markAndGo("norm! W", "W")end, "WORD")
nnoremap("]E", function() markAndGo("norm! E", "E")end, "WORD End")
nnoremap("]$w", function() markAndGo("norm $w)", "$w")end, "Sub-Word")
nnoremap("]$e", function() markAndGo("norm $e", "$e")end, "Sub-Word End")
xnoremap("]p", function() markGoCentre("norm! }", "p")end, "Paragraph")
xnoremap("]P", function() markGoCentre("call v:lua.paragraph_end_jump(v:count)", "P")end, "Paragraph")
xnoremap("]w", function() markAndGo("norm! w", "w")end, "Word")
xnoremap("]e", function() markAndGo("norm! e", "e")end, "Word")
xnoremap("]W", function() markAndGo("norm! W", "W")end, "WORD")
xnoremap("]E", function() markAndGo("norm! E", "E")end, "WORD End")
xnoremap("]$w", function() markAndGo("norm $w)", "$w")end, "Sub-Word")
xnoremap("]$e", function() markAndGo("norm $e", "$e")end, "Sub-Word End")
onoremap("]p", function() markAndGo("norm! }", "p")end, "Paragraph")
onoremap("]P", function() markGoCentre("call v:lua.paragraph_end_jump(v:count)", "P")end, "Paragraph")
onoremap("]w", function() markAndGo("norm! w", "w")end, "Word")
onoremap("]e", function() markAndGo("norm! e", "e")end, "Word End")
onoremap("]W", function() markAndGo("norm! W", "W")end, "WORD")
onoremap("]E", function() markAndGo("norm! E", "E")end, "WORD End")
onoremap("]$w", function() markAndGo("norm $w", "$w")end, "Sub-Word")
onoremap("]$e", function() markAndGo("norm $e", "$e")end, "Sub-Word End")

nnoremap("]d", function() markGoCentre("lua vim.diagnostic.goto_next({float={border='single',scope='cursor',source='always'}})", "d")end, "Diagnostics")
    mapxName.name("]D", "Diagnostaic Type")
    nnoremap("]Dn", function() markGoCentre("lua vim.diagnostic.goto_next({severity='Error',float={border='single',scope='cursor',source='always'}})", "Da")end, "Error")
    nnoremap("]De", function() markGoCentre("lua vim.diagnostic.goto_next({severity='Warn',float={border='single',scope='cursor',source='always'}})", "Dr")end, "Warn")
    nnoremap("]Di", function() markGoCentre("lua vim.diagnostic.goto_next({severity='Info',float={border='single',scope='cursor',source='always'}})", "Ds")end, "Info")
    nnoremap("]Do", function() markGoCentre("lua vim.diagnostic.goto_next({severity='Hint',float={border='single',scope='cursor',source='always'}})", "Dt")end, "Hint")
xnoremap("]d", function() markGoCentre("lua vim.diagnostic.goto_next({float={border='single',scope='cursor',source='always'}})", "d")end, "Diagnostics")
    xnoremap("]Dn", function() markGoCentre("lua vim.diagnostic.goto_next({severity='Error',float={border='single',scope='cursor',source='always'}})", "Da")end, "Error")
    xnoremap("]De", function() markGoCentre("lua vim.diagnostic.goto_next({severity='Warn',float={border='single',scope='cursor',source='always'}})", "Dr")end, "Warn")
    xnoremap("]Di", function() markGoCentre("lua vim.diagnostic.goto_next({severity='Info',float={border='single',scope='cursor',source='always'}})", "Ds")end, "Info")
    xnoremap("]Do", function() markGoCentre("lua vim.diagnostic.goto_next({severity='Hint',float={border='single',scope='cursor',source='always'}})", "Dt")end, "Hint")
onoremap("]d", function() markAndGo("lua vim.diagnostic.goto_next({float={border='single',scope='cursor',source='always'}})", "d")end, "Diagnostics")
    onoremap("]Dn", function() markAndGo("lua vim.diagnostic.goto_next({severity='Error',float={border='single',scope='cursor',source='always'}})", "Da")end, "Error")
    onoremap("]De", function() markAndGo("lua vim.diagnostic.goto_next({severity='Warn',float={border='single',scope='cursor',source='always'}})", "Dr")end, "Warn")
    onoremap("]Di", function() markAndGo("lua vim.diagnostic.goto_next({severity='Info',float={border='single',scope='cursor',source='always'}})", "Ds")end, "Info")
    onoremap("]Do", function() markAndGo("lua vim.diagnostic.goto_next({severity='Hint',float={border='single',scope='cursor',source='always'}})", "Dt")end, "Hint")

if vim.api.nvim_win_get_option(0, "diff") then
    nnoremap("[h", function() markGoCentre("norm! [c", "h")end, "Git Diff")
    nnoremap("]h", function() markGoCentre("norm! ]c", "h")end, "Git Diff")
    xnoremap("[h", function() markGoCentre("norm! [c", "h")end, "Git Diff")
    xnoremap("]h", function() markGoCentre("norm! ]c", "h")end, "Git Diff")
    onoremap("[h", function() markAndGo("norm! [c", "h")end, "Git Diff")
    onoremap("]h", function() markAndGo("norm! ]c", "h")end, "Git Diff")
end

mapxName.xname("a", "Around")
xnoremap("ah", ":<c-u>Gitsigns selct_hunk<cr>", "Git Hunk")
xnoremap("ai", ":<c-u>TSTextobjectSelect @block.outer<cr>", "Block")
xnoremap("a,", ":<c-u>TSTextobjectSelect @parameter.outer<cr>", "Parameter")
xnoremap("ac", ":<c-u>TSTextobjectSelect @comment.outer<cr>", "Comment")
xnoremap("as", ":<c-u>TSTextobjectSelect @function.outer<cr>", "Function")
xnoremap("av", "<cmd>exec 'normal! HVL'<cr>", "Select Viewport")
xnoremap("af", "<cmd>exec 'normal! ggVG'<cr>", "Select File")
    mapxName.xname("a]", "Around Next")
    xnoremap("a]w", ":<c-u>call v:lua.mapped_targets(v:count, 'w', 'aw')<cr>", "Word")
    xnoremap("a]W", ":<c-u>call v:lua.mapped_targets(v:count, 'W', 'aW')<cr>", "Word")
    xnoremap("a]$w", ":<c-u>call v:lua.mapped_targets(v:count, '$w', 'a$w')<cr>", "Sub Word")
    xnoremap("a]p", ":<c-u>call v:lua.paragraph_targets(v:count, 1)<cr>", "Paragraph")
    xnoremap("a]h", ":<c-u>call v:lua.git_target(v:count, 'true')<cr>", "Git Hunk")
    xnoremap("a]i", ":<c-u>call v:lua.ts_target(v:count, '@block.outer')<cr>", "Block")
    xnoremap("a],", ":<c-u>call v:lua.ts_target(v:count, '@parameter.outer')<cr>", "Parameter")
    xnoremap("a]c", ":<c-u>call v:lua.ts_target(v:count, '@comment.outer')<cr>", "Comment")
    xnoremap("a]s", ":<c-u>call v:lua.ts_target(v:count, '@function.outer')<cr>", "Function")
    mapxName.xname("a[", "Around Previous")
    xnoremap("a[w", ":<c-u>call v:lua.mapped_targets_back(v:count, 'b', 'ge', 'aw')<cr>", "Word")
    xnoremap("a[W", ":<c-u>call v:lua.mapped_targets_back(v:count, 'B', 'gE', 'aW')<cr>", "Word")
    xnoremap("a[$w", ":<c-u>call v:lua.mapped_targets_back(v:count, '$ge', '$b', 'a$w')<cr>", "Sub Word")
    xnoremap("a[p", ":<c-u>call v:lua.paragraph_targets_back(v:count, 1)<cr>", "Paragraph")
    xnoremap("a[h", ":<c-u>call v:lua.git_target(v:count, 'false')<cr>", "Git Hunk")
    xnoremap("a[i", ":<c-u>call v:lua.ts_target_back(v:count, '@block.outer')<cr>", "Block")
    xnoremap("a[,", ":<c-u>call v:lua.ts_target_back(v:count, '@parameter.outer')<cr>", "Parameter")
    xnoremap("a[c", ":<c-u>call v:lua.ts_target_back(v:count, '@comment.outer')<cr>", "Comment")
    xnoremap("a[s", ":<c-u>call v:lua.ts_target_back(v:count, '@function.outer')<cr>", "Function")
mapxName.xname("i", "In")
xnoremap("ih", ":<c-u>Gitsigns selct_hunk<cr>", "Git Hunk")
xnoremap("ii", ":<c-u>TSTextobjectSelect @block.inner<cr>", "Block")
xnoremap("i,", ":<c-u>TSTextobjectSelect @parameter.inner<cr>", "Parameter")
xnoremap("ic", ":<c-u>TSTextobjectSelect @comment.outer<cr>", "Comment")
xnoremap("is", ":<c-u>TSTextobjectSelect @function.inner<cr>", "Function")
xnoremap("iv", "<cmd>exec 'normal! HVL'<cr>", "Select Viewport")
xnoremap("ie", "<cmd>exec 'normal! ggVG'<cr>", "Select File")
    mapxName.xname("i[", "In Next")
    xnoremap("i]w", ":<c-u>call v:lua.mapped_targets(v:count, 'w', 'iw')<cr>", "Word")
    xnoremap("i]W", ":<c-u>call v:lua.mapped_targets(v:count, 'W', 'iW')<cr>", "WORD")
    xnoremap("i]$w", ":<c-u>call v:lua.mapped_targets(v:count, '$w', 'i$w')<cr>", "Sub Word")
    xnoremap("i]p", ":<c-u>call v:lua.paragraph_targets(v:count, 0)<cr>", "Paragraph")
    xnoremap("i]h", ":<c-u>call v:lua.git_target(v:count, 'true')<cr>", "Git Hunk")
    xnoremap("i]i", ":<c-u>call v:lua.ts_target(v:count, '@block.inner')<cr>", "Block")
    xnoremap("i],", ":<c-u>call v:lua.ts_target(v:count, '@parameter.inner')<cr>", "Parameter")
    xnoremap("i]c", ":<c-u>call v:lua.ts_target(v:count, '@comment.outer')<cr>", "Comment")
    xnoremap("i]s", ":<c-u>call v:lua.ts_target(v:count, '@function.inner')<cr>", "Function")
    mapxName.xname("i[", "In Previous")
    xnoremap("i[w", ":<c-u>call v:lua.mapped_targets_back(v:count, 'b', 'ge', 'iw')<cr>", "Word")
    xnoremap("i[W", ":<c-u>call v:lua.mapped_targets_back(v:count, 'B', 'gE', 'iW')<cr>", "WORD")
    xnoremap("i[$w", ":<c-u>call v:lua.mapped_targets_back(v:count, '$ge', '$b', 'i$w')<cr>", "Sub Word")
    xnoremap("i[p", ":<c-u>call v:lua.paragraph_targets_back(v:count, 0)<cr>", "Paragraph")
    xnoremap("i[h", ":<c-u>call v:lua.git_target(v:count, 'false')<cr>", "Git Hunk")
    xnoremap("i[i", ":<c-u>call v:lua.ts_target_back(v:count, '@block.inner')<cr>", "Block")
    xnoremap("i[,", ":<c-u>call v:lua.ts_target_back(v:count, '@parameter.inner')<cr>", "Parameter")
    xnoremap("i[c", ":<c-u>call v:lua.ts_target_back(v:count, '@comment.outer')<cr>", "Comment")
    xnoremap("i[s", ":<c-u>call v:lua.ts_target_back(v:count, '@function.inner')<cr>", "Function")

mapxName.oname("a", "Around")
omap("an", "v:lua.commandRepeat('a]', 'dirJumps')", "Around Last Jump", expr)
omap("aN", "v:lua.commandRepeat('a[', 'dirJumps')", "Around Next Jump", expr)
onoremap("ah", ":<c-u>Gitsigns selct_hunk<cr>", "Git Hunk")
onoremap("ai", ":<c-u>TSTextobjectSelect @block.outer<cr>", "Block")
onoremap("a,", ":<c-u>TSTextobjectSelect @parameter.outer<cr>", "Parameter")
onoremap("ac", ":<c-u>TSTextobjectSelect @comment.outer<cr>", "Comment")
onoremap("as", ":<c-u>TSTextobjectSelect @function.outer<cr>", "Function")
onoremap("av", "<cmd>exec 'normal! HVL'<cr>", "Select Viewport")
onoremap("af", "<cmd>exec 'normal! ggVG'<cr>", "Select File")
    mapxName.oname("a]", "Around Next")
    onoremap("a]w", ":<c-u>call v:lua.mapped_targets(v:count, 'w', 'aw')<cr>", "Word")
    onoremap("a]W", ":<c-u>call v:lua.mapped_targets(v:count, 'W', 'aW')<cr>", "Word")
    onoremap("a]$w", ":<c-u>call v:lua.mapped_targets(v:count, '$w', 'a$w')<cr>", "Sub Word")
    onoremap("a]p", ":<c-u>call v:lua.paragraph_targets(v:count, 1)<cr>", "Paragraph")
    onoremap("a]h", ":<c-u>call v:lua.git_target(v:count, 'true')<cr>", "Git Hunk")
    onoremap("a]i", ":<c-u>call v:lua.ts_target(v:count, '@block.outer')<cr>", "Block")
    onoremap("a],", ":<c-u>call v:lua.ts_target(v:count, '@parameter.outer')<cr>", "Parameter")
    onoremap("a]c", ":<c-u>call v:lua.ts_target(v:count, '@comment.outer')<cr>", "Comment")
    onoremap("a]s", ":<c-u>call v:lua.ts_target(v:count, '@function.outer')<cr>", "Function")
    onoremap("a]o", ":<c-u>call v:lua.ts_target(v:count, '@class.outer')<cr>", "Class")
    mapxName.oname("a[", "Around Previous")
    onoremap("a[w", ":<c-u>call v:lua.mapped_targets_back(v:count, 'b', 'ge', 'aw')<cr>", "Word")
    onoremap("a[W", ":<c-u>call v:lua.mapped_targets_back(v:count, 'B', 'gE', 'aW')<cr>", "Word")
    onoremap("a[$w", ":<c-u>call v:lua.mapped_targets_back(v:count, '$ge', '$b', 'a$w')<cr>", "Sub Word")
    onoremap("a[p", ":<c-u>call v:lua.paragraph_targets_back(v:count, 1)<cr>", "Paragraph")
    onoremap("a[h", ":<c-u>call v:lua.git_target(v:count, 'false')<cr>", "Git Hunk")
    onoremap("a[i", ":<c-u>call v:lua.ts_target_back(v:count, '@block.outer')<cr>", "Block")
    onoremap("a[,", ":<c-u>call v:lua.ts_target_back(v:count, '@parameter.outer')<cr>", "Parameter")
    onoremap("a[c", ":<c-u>call v:lua.ts_target_back(v:count, '@comment.outer')<cr>", "Comment")
    onoremap("a[s", ":<c-u>call v:lua.ts_target_back(v:count, '@function.outer')<cr>", "Function")
mapxName.oname("i", "In")
omap("in", "v:lua.commandRepeat('i]', 'dirJumps')", "In Last Jump", expr)
omap("iN", "v:lua.commandRepeat('i[', 'dirJumps')", "In Next Jump", expr)
onoremap("ih", ":<c-u>Gitsigns selct_hunk<cr>", "Git Hunk")
onoremap("ii", ":<c-u>TSTextobjectSelect @block.inner<cr>", "Block")
onoremap("i,", ":<c-u>TSTextobjectSelect @parameter.inner<cr>", "Parameter")
onoremap("ic", ":<c-u>TSTextobjectSelect @comment.outer<cr>", "Comment")
onoremap("is", ":<c-u>TSTextobjectSelect @function.inner<cr>", "Function")
onoremap("iv", "<cmd>exec 'normal! HVL'<cr>", "Select Viewport")
onoremap("if", "<cmd>exec 'normal! ggVG'<cr>", "Select File")
    mapxName.oname("i[", "In Next")
    onoremap("i]w", ":<c-u>call v:lua.mapped_targets(v:count, 'w', 'iw')<cr>", "Word")
    onoremap("i]W", ":<c-u>call v:lua.mapped_targets(v:count, 'W', 'iW')<cr>", "WORD")
    onoremap("i]$w", ":<c-u>call v:lua.mapped_targets(v:count, '$w', 'i$w')<cr>", "Sub Word")
    onoremap("i]p", ":<c-u>call v:lua.paragraph_targets(v:count, 0)<cr>", "Paragraph")
    onoremap("i]h", ":<c-u>call v:lua.git_target(v:count, 'true')<cr>", "Git Hunk")
    onoremap("i]i", ":<c-u>call v:lua.ts_target(v:count, '@block.inner')<cr>", "Block")
    onoremap("i],", ":<c-u>call v:lua.ts_target(v:count, '@parameter.inner')<cr>", "Parameter")
    onoremap("i]c", ":<c-u>call v:lua.ts_target(v:count, '@comment.outer')<cr>", "Comment")
    onoremap("i]s", ":<c-u>call v:lua.ts_target(v:count, '@function.inner')<cr>", "Function")
    mapxName.oname("i[", "In Previous")
    onoremap("i[w", ":<c-u>call v:lua.mapped_targets_back(v:count, 'b', 'ge', 'iw')<cr>", "Word")
    onoremap("i[W", ":<c-u>call v:lua.mapped_targets_back(v:count, 'B', 'gE', 'iW')<cr>", "WORD")
    onoremap("i[$w", ":<c-u>call v:lua.mapped_targets_back(v:count, '$ge', '$b', 'i$w')<cr>", "Sub Word")
    onoremap("i[p", ":<c-u>call v:lua.paragraph_targets_back(v:count, 0)<cr>", "Paragraph")
    onoremap("i[h", ":<c-u>call v:lua.git_target(v:count, 'false')<cr>", "Git Hunk")
    onoremap("i[i", ":<c-u>call v:lua.ts_target_back(v:count, '@block.inner')<cr>", "Block")
    onoremap("i[,", ":<c-u>call v:lua.ts_target_back(v:count, '@parameter.inner')<cr>", "Parameter")
    onoremap("i[c", ":<c-u>call v:lua.ts_target_back(v:count, '@comment.outer')<cr>", "Comment")
    onoremap("i[s", ":<c-u>call v:lua.ts_target_back(v:count, '@function.inner')<cr>", "Function")

local function replace_keycodes(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end
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
