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
    }
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

local home = vim.fn.expand("~/UniversityDrive/Thesis/notes")
require('telekasten').setup({
    home         = home,
    take_over_my_home = true,
    auto_set_filetype = true,

    dailies      = home .. '/' .. 'daily',
    weeklies     = home .. '/' .. 'weekly',
    templates    = home .. '/' .. 'templates',
    image_subdir = "img",
    extension    = ".md",

    image_link_style = "wiki",
})
