vim.b[0].localCommands = {
	{ source = "tex", name = "Build project", command = "VimtexCompileSS" },
	{ source = "tex", name = "Table of content", command = "VimtexTocToggle" },
	{ source = "tex", name = "Forward Search", command = "TexlabForward" },
	{ source = "tex", name = "Table of labels", command = "call g:custom_toc1.toggle()" },
	{ source = "tex", name = "Table of todos", command = "call g:custom_toc2.toggle()" },
	{ source = "tex", name = "Word Count", command = "VimtexCountWord" },
}

Map("n", "<localleader><localleader>", "<cmd>TexlabForward<cr>", { buffer = 0, silent = true })
Map("n", "<leader>d", "<cmd>VimtexCompileSS<cr>", { buffer = 0, silent = true })

Map("n", "KK", "<cmd>VimtexDocPackage<cr>", { buffer = 0 })

Map("n", "[s", "<cmd>let g:dirJumps='s'<cr>m`<plug>(vimtex-[[)zz", { buffer = 0 })
Map("n", "[S", "<cmd>let g:dirJumps='S'<cr>m`<plug>(vimtex-[])zz", { buffer = 0 })
Map("n", "[m", "<cmd>let g:dirJumps='m'<cr>m`<plug>(vimtex-[m)zz", { buffer = 0 })
Map("n", "[M", "<cmd>let g:dirJumps='M'<cr>m`<plug>(vimtex-[M)zz", { buffer = 0 })
Map("n", "[n", "<cmd>let g:dirJumps='n'<cr>m`<plug>(vimtex-[n)zz", { buffer = 0 })
Map("n", "[N", "<cmd>let g:dirJumps='N'<cr>m`<plug>(vimtex-[N)zz", { buffer = 0 })
Map("n", "[r", "<cmd>let g:dirJumps='r'<cr>m`<plug>(vimtex-[r)zz", { buffer = 0 })
Map("n", "[R", "<cmd>let g:dirJumps='R'<cr>m`<plug>(vimtex-[R)zz", { buffer = 0 })
Map("n", "[]", "<cmd>let g:dirJumps='S'<cr>m`<plug>(vimtex-[[)zz", { buffer = 0 })
Map("n", "[[", "<cmd>let g:dirJumps='s'<cr>m`<plug>(vimtex-[])zz", { buffer = 0 })

Map("n", "]s", "<cmd>let g:dirJumps='s'<cr>m`<plug>(vimtex-]])zz", { buffer = 0 })
Map("n", "]S", "<cmd>let g:dirJumps='S'<cr>m`<plug>(vimtex-][)zz", { buffer = 0 })
Map("n", "]m", "<cmd>let g:dirJumps='m'<cr>m`<plug>(vimtex-]m)zz", { buffer = 0 })
Map("n", "]M", "<cmd>let g:dirJumps='M'<cr>m`<plug>(vimtex-]M)zz", { buffer = 0 })
Map("n", "]n", "<cmd>let g:dirJumps='n'<cr>m`<plug>(vimtex-]n)zz", { buffer = 0 })
Map("n", "]N", "<cmd>let g:dirJumps='N'<cr>m`<plug>(vimtex-]N)zz", { buffer = 0 })
Map("n", "]r", "<cmd>let g:dirJumps='r'<cr>m`<plug>(vimtex-]r)zz", { buffer = 0 })
Map("n", "]R", "<cmd>let g:dirJumps='R'<cr>m`<plug>(vimtex-]R)zz", { buffer = 0 })
Map("n", "][", "<cmd>let g:dirJumps='s'<cr>m`<plug>(vimtex-][)zz", { buffer = 0 })
Map("n", "]]", "<cmd>let g:dirJumps='S'<cr>m`<plug>(vimtex-]])zz", { buffer = 0 })

Map({ "x", "o" }, "iP", "<plug>(vimtex-iP)", { buffer = 0, remap = true })
Map({ "x", "o" }, "im", "<plug>(vimtex-ie)", { buffer = 0, remap = true })
Map({ "x", "o" }, "in", "<plug>(vimtex-i$)", { buffer = 0, remap = true })
Map({ "x", "o" }, "aP", "<plug>(vimtex-aP)", { buffer = 0, remap = true })
Map({ "x", "o" }, "am", "<plug>(vimtex-ae)", { buffer = 0, remap = true })
Map({ "x", "o" }, "an", "<plug>(vimtex-a$)", { buffer = 0, remap = true })
Map({ "x", "o" }, "i]P", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]])', '(vimtex-iP)')<cr>", { buffer = 0 })
Map({ "x", "o" }, "i]m", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]m)', '(vimtex-ie)')<cr>", { buffer = 0 })
Map({ "x", "o" }, "i]n", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]n)', '(vimtex-i$)')<cr>", { buffer = 0 })
Map(
	{ "x", "o" },
	"i[P",
	":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[])', '(vimtex-[])', '(vimtex-iP)')<cr>",
	{ buffer = 0 }
)
Map(
	{ "x", "o" },
	"i[m",
	":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[m)', '(vimtex-[M)', '(vimtex-ie)')<cr>",
	{ buffer = 0 }
)
Map(
	{ "x", "o" },
	"i[n",
	":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[n)', '(vimtex-[N)', '(vimtex-i$)')<cr>",
	{ buffer = 0 }
)
Map({ "x", "o" }, "a]P", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]])', '(vimtex-aP)')<cr>", { buffer = 0 })
Map({ "x", "o" }, "a]m", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]m)', '(vimtex-ae)')<cr>", { buffer = 0 })
Map({ "x", "o" }, "a]n", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]n)', '(vimtex-a$)')<cr>", { buffer = 0 })
Map(
	{ "x", "o" },
	"a[P",
	":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[])', '(vimtex-[])', '(vimtex-aP)')<cr>",
	{ buffer = 0 }
)
Map(
	{ "x", "o" },
	"a[m",
	":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[m)', '(vimtex-[M)', '(vimtex-ae)')<cr>",
	{ buffer = 0 }
)
Map(
	{ "x", "o" },
	"a[n",
	":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[n)', '(vimtex-[N)', '(vimtex-a$)')<cr>",
	{ buffer = 0 }
)

if vim.api.nvim_get_var("panelRepeat") == "q" then
	vim.api.nvim_set_var("panelRepeat", "c")
end

vim.api.nvim_buf_set_option(0, "textwidth", 100)

-- vim.cmd([[let g:vimtex_toc_config=[{"mode":3, "split_width":60}]]])
vim.cmd([[let g:vimtex_quickfix_mode=0]])
vim.cmd([[let g:vimtex_view_general_viewer='zathura']])
vim.cmd([[let g:vimtex_view_forward_search_on_start=1]])
vim.cmd([[let g:vimtex_view_automatic = 0]])
vim.g.vimtex_compiler_latexmk = {
	["callback"] = 1,
	["continuous"] = 0,
	["executable"] = "latexmk",
	["hooks"] = {},
	["options"] = { "-verbose", "-file-line-error", "-synctex=1", "-interaction=nonstopmode" },
}

vim.opt_local.conceallevel = 2
vim.g.vimtex_syntax_conceal = {
	["accents"] = true,
	["cites"] = true,
	["fancy"] = true,
	["greek"] = true,
	["math_bounds"] = true,
	["math_delimiters"] = true,
	["math_fracs"] = true,
	["math_super_sub"] = false,
	["math_symbols"] = true,
	["sections"] = true,
	["styles"] = true,
}
vim.cmd([[
    let g:custom_toc1 = vimtex#toc#new({ 'name':'Tabel of labels', 'layers' : ['label'], 'todo_sorted' : 0, 'show_help' : 0, 'show_numbers' : 0, 'mode' : 1})
    let g:custom_toc2 = vimtex#toc#new({ 'name':'TODO', 'layers' : ['todo'], 'todo_sorted' : 0, 'show_help' : 0, 'show_numbers' : 0, 'mode' : 1})
]])

vim.g.AngryReviewerEnglish = "british"

vim.g.projectionist_heuristics = {
	["OML-Thesis.tex"] = {
		["1.tex"] = {
			type = "chapter",
			alternate = "OML-Thesis.tex",
		},
		["2.tex"] = {
			type = "chapter",
			alternate = "OML-Thesis.tex",
		},
		["3.tex"] = {
			type = "chapter",
			alternate = "OML-Thesis.tex",
		},
		["4.tex"] = {
			type = "chapter",
			alternate = "OML-Thesis.tex",
		},
		["5.tex"] = {
			type = "chapter",
			alternate = "OML-Thesis.tex",
		},
		["6.tex"] = {
			type = "chapter",
			alternate = "OML-Thesis.tex",
		},
		["7.tex"] = {
			type = "chapter",
			alternate = "OML-Thesis.tex",
		},
		["Appendix.tex"] = {
			type = "chapter",
			alternate = "OML-Thesis.tex",
		},
		["*.tex"] = {
			type = "section",
			alternate = "{dirname}.tex",
		},
		["Scripts/*.tex"] = {
			type = "scripts",
			alternate = "OML-Thesis.tex",
		},
		["Scripts/Packages.tex"] = { type = "deps" },
		["IntroductiontoAM.tex"] = { type = "chapOneMain" },
		["PowderBedFusion.tex"] = { type = "chapTwoMain" },
		["ModellingofAM.tex"] = { type = "chapThreeMain" },
		["PowderCharacterisation.tex"] = { type = "chapFourMain" },
		["PowderModel.tex"] = { type = "chapFiveMain" },
		["MachineModel.tex"] = { type = "chapSixMain" },
		["ProcessModel.tex"] = { type = "chapSevenMain" },
		["OML-Thesis.tex"] = { type = "main" },
		["Citations.bib"] = { type = "citations" },
	},
}

if vim.g.viewerOpen ~= 1 then
	vim.g.viewerOpen = 1
	vim.cmd("VimtexView")
	vim.cmd("sleep 200m")
	vim.cmd("silent !xdotool key super+n")
end
