-- Targets Setup
vim.cmd([[augroup mywellle
    autocmd!
    autocmd User targets#mappings#user call targets#mappings#extend({
        \ 'z': { 'separator': [{'d':','}, {'d':'.'}, {'d':';'}, {'d':':'}, {'d':'+'}, {'d':'-'},
        \                      {'d':'='}, {'d':'~'}, {'d':'_'}, {'d':'*'}, {'d':'#'}, {'d':'/'},
        \                      {'d':'\'}, {'d':'|'}, {'d':'&'}, {'d':'$'}] },
        \ ',': {},
        \ ';': {},
        \ ':': {},
        \ '+': {},
        \ '-': {},
        \ '=': {},
        \ '~': {},
        \ '_': {},
        \ '*': {},
        \ '#': {},
        \ '/': {},
        \ '\': {},
        \ '|': {},
        \ '&': {},
        \ '$': {},
        \ 'a': {'argument': [{'o': '[{([]', 'c': '[])}]', 's': '[,;]'}]},
        \ 'x': {'line': [{'c': 1}]},
        \ 't': {'tag': [{}]},
        \ '<': {'pair': [{'o': '<', 'c': '>'}]},
        \ '>': {'pair': [{'o': '>', 'c': '<'}]},
        \ 'b': {'pair': [{'o':'(', 'c':')'}, {'o':'[', 'c':']'}, {'o':'{', 'c':'}'}]},
        \ 'q': {'quote': [{'d':"'"}, {'d':'"'}, {'d':'`'}]},
        \ })
augroup end]])
vim.g.matchup_matchparen_offscreen = { method = false }
vim.g.matchup_text_obj_enabled = false

vim.api.nvim_set_var("targets_seekRanges", "cc cr cb cB lc ac Ac lr rr ll lb ar ab lB Ar aB Ab AB rb rB al Al")
vim.api.nvim_set_var("targets_jumpRanges", "rr rb rB bb bB BB ll al Al aa Aa AA")
vim.api.nvim_set_var("targets_gracious", true)
vim.api.nvim_set_var("targets_nl", "][")
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

require("Comment").setup({
	ignore = "^$",
	toggler = {
		line = ",cc",
		block = ",bb",
	},
	opleader = {
		line = ",c",
		block = ",b",
	},
	extra = {
		above = ",cO",
		below = ",co",
		eol = ",cA",
	},
	mappings = {
		basic = true,
		extra = true,
		extended = false,
	},
})

require("hop").setup({ keys = "tnseriaodhgjplfuwybkvmcxzq" })
require("colorizer").setup({ "*" }, {
	RGB = true,
	RRGGBB = true,
	names = false,
	RRGGBBAA = true,
	rgb_fn = true,
	hsl_fn = true,
	css_fn = false,
	mode = "background",
})

local perfanno = require("perfanno")
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
