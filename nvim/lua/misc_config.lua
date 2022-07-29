vim.g.matchup_matchparen_offscreen = { method = false }
vim.g.matchup_text_obj_enabled = false
vim.cmd([[fu! s:lastplace()
	if index(split("quickfix,nofile,help", ","), &buftype) != -1  | return | endif
	if index(split("gitcommit,gitrebase,svn,hgcommit", ","), &filetype) != -1 | return | endif
	try |
		if empty(glob(@%)) | return | endif
	catch | return | endtry
	if line("'\"") > 0 && line("'\"") <= line("$") | execute "normal! g`\"zz" | endif
endf
augroup lastplace_notplugin
	autocmd!
	autocmd BufWinEnter * call s:lastplace()
augroup END]])

-- Close Buffers Setup
require("close_buffers").setup({
    preserve_window_layout = { "this" },
})

require("hop").setup({ keys = "tnseriaodhgjplfuwybkvmcxzq" })

local util = require("perfanno.util")

require("perfanno").setup({
    line_highlights = util.make_bg_highlights("#1F1F28", "#C34043", 10),
    vt_highlight = util.make_fg_highlights("#DCD7BA", "#C34043", 10),
    formats = {
        { percent = true, format = "%.2f%%", minimum = 0.0 },
        { percent = false, format = "%d", minimum = 0.0001 },
    },
})
require("coverage").setup({
    signs = {
        covered = { hl = "CoverageCovered", text = "▉" },
        uncovered = { hl = "CoverageUncovered", text = "▉" },
    },
})
