require("which-key").register({
	["<localleader>"] = {
		["<localleader>"] = { "<cmd>TexlabForward<cr>", "Forward Search", silent = true },
		r = { "<cmd>Telescope bibtex bibtex theme=get_cursor<cr>", "References" },
		c = { "<cmd>VimtexTocToggle<cr>", "Open TOC" },
		v = { "<cmd>VimtexView<cr>", "View Document" },
		w = { "<cmd>VimtexCountWord<cr>", "Word Count" },
		W = { "<cmd>VimtexCountWord!<cr>", "Word Count Report" },
		m = { "<cmd>VimtexToggleMain<cr>", "Toggle the Main File" },
		g = { "<cmd>TexlabForward<cr>", "Forward Search" },
		i = { "A% chktex ", "Chktex Ignore" },
        s = { "<cmd>lua require'telescope.builtin'.symbols{sources={'latex'}}<cr>", "Insert Symbols"},
	},
	["<leader>"] = {
		["/"] = {
			["1"] = { "<cmd>EchapOneMain<cr>", "Chapter 1" },
			["2"] = { "<cmd>EchapTwoMain<cr>", "Chapter 2" },
			["3"] = { "<cmd>EchapThreeMain<cr>", "Chapter 3" },
			["4"] = { "<cmd>EchapFourMain<cr>", "Chapter 4" },
			["5"] = { "<cmd>EchapFiveMain<cr>", "Chapter 5" },
			["6"] = { "<cmd>EchapSixMain<cr>", "Chapter 6" },
			["7"] = { "<cmd>EchapSevenMain<cr>", "Chapter 7" },
			c = { "<cmd>Ecitations<cr>", "Citations" },
			m = { "<cmd>Emain<cr>", "Main File" },
			v = {
				["1"] = { "<cmd>VchapOneMain<cr>", "Chapter 1" },
				["2"] = { "<cmd>VchapTwoMain<cr>", "Chapter 2" },
				["3"] = { "<cmd>VchapThreeMain<cr>", "Chapter 3" },
				["4"] = { "<cmd>VchapFourMain<cr>", "Chapter 4" },
				["5"] = { "<cmd>VchapFiveMain<cr>", "Chapter 5" },
				["6"] = { "<cmd>VchapSixMain<cr>", "Chapter 6" },
				["7"] = { "<cmd>VchapSevenMain<cr>", "Chapter 7" },
				c = { "<cmd>Vcitations<cr>", "Citations" },
				m = { "<cmd>Vmain<cr>", "Main File" },
			},
			x = {
				["1"] = { "<cmd>SchapOneMain<cr>", "Chapter 1" },
				["2"] = { "<cmd>SchapTwoMain<cr>", "Chapter 2" },
				["3"] = { "<cmd>SchapThreeMain<cr>", "Chapter 3" },
				["4"] = { "<cmd>SchapFourMain<cr>", "Chapter 4" },
				["5"] = { "<cmd>SchapFiveMain<cr>", "Chapter 5" },
				["6"] = { "<cmd>SchapSixMain<cr>", "Chapter 6" },
				["7"] = { "<cmd>SchapSevenMain<cr>", "Chapter 7" },
				c = { "<cmd>Scitations<cr>", "Citations" },
				m = { "<cmd>Smain<cr>", "Main File" },
			},
			T = {
				["1"] = { "<cmd>TchapOneMain<cr>", "Chapter 1" },
				["2"] = { "<cmd>TchapTwoMain<cr>", "Chapter 2" },
				["3"] = { "<cmd>TchapThreeMain<cr>", "Chapter 3" },
				["4"] = { "<cmd>TchapFourMain<cr>", "Chapter 4" },
				["5"] = { "<cmd>TchapFiveMain<cr>", "Chapter 5" },
				["6"] = { "<cmd>TchapSixMain<cr>", "Chapter 6" },
				["7"] = { "<cmd>TchapSevenMain<cr>", "Chapter 7" },
				c = { "<cmd>Tcitations<cr>", "Citations" },
				m = { "<cmd>Tmain<cr>", "Main File" },
			},
			n = {
				["1"] = { [["<cmd>EchapOne " . input('File Name >') . "<cr>"]], "Chapter 1", expr = true },
				["2"] = { [["<cmd>EchapTwo " . input('File Name >') . "<cr>"]], "Chapter 2", expr = true },
				["3"] = { [["<cmd>EchapThree " . input('File Name >') . "<cr>"]], "Chapter 3", expr = true },
				["4"] = { [["<cmd>EchapFour " . input('File Name >') . "<cr>"]], "Chapter 4", expr = true },
				["5"] = { [["<cmd>EchapFive " . input('File Name >') . "<cr>"]], "Chapter 5", expr = true },
				["6"] = { [["<cmd>EchapSix " . input('File Name >') . "<cr>"]], "Chapter 6", expr = true },
				["7"] = { [["<cmd>EchapSeven " . input('File Name >') . "<cr>"]], "Chapter 7", expr = true },
				u = { [["<cmd>Escripts " . input('File Name >') . "<cr>"]], "Scripts", expr = true },
			},
		},
		M = {
			[[<cmd>2TermExec cmd="latexmk -verbose -file-line-error -synctex=1 -interaction=nonstopmode"<cr>]],
			"Build in Terminal",
		},
		m = {
			m = { "<cmd>VimtexCompileSS<cr>", "Build Once" },
			M = { "<cmd>ProjectDo TexlabBuild<cr>", "Build Project" },
			o = { "<cmd>VimtexCompile<cr>", "Toggle Continuous Building" },
			c = { "<cmd>VimtexClean<cr>", "Clear Build Files" },
			g = { "<cmd>compiler textidote<cr><cmd>lmake!<cr><cmd>Trouble loclist<cr>", "Grammer Checking" },
		},
        v = {
            c = { "<cmd>let panelRepeat='c'<cr><cmd>VimtexTocToggle<cr>", "Open TOC" },
        },
	},
    ["["] = {
		s = { "<cmd>let g:dirJumps='s'<cr><plug>(vimtex-[[)zz", "Tex Section End", noremap = false},
		S = { "<cmd>let g:dirJumps='S'<cr><plug>(vimtex-[])zz", "Tex Section Start", noremap = false},
		m = { "<cmd>let g:dirJumps='m'<cr><plug>(vimtex-[m)zz", "EnviroMent Start", noremap = false},
		M = { "<cmd>let g:dirJumps='M'<cr><plug>(vimtex-[M)zz", "EnviroMent End", noremap = false},
		n = { "<cmd>let g:dirJumps='n'<cr><plug>(vimtex-[n)zz", "Number Start", noremap = false},
		N = { "<cmd>let g:dirJumps='N'<cr><plug>(vimtex-[N)zz", "Number End", noremap = false},
		r = { "<cmd>let g:dirJumps='r'<cr><plug>(vimtex-[r)zz", "Frame Start", noremap = false},
		R = { "<cmd>let g:dirJumps='R'<cr><plug>(vimtex-[R)zz", "Frame End", noremap = false},
		C = { "<cmd>let g:dirJumps='C'<cr><plug>(vimtex-[/)zz", "Comment Start", noremap = false},
    },
    ["]"] = {
		s = { "<cmd>let g:dirJumps='s'<cr><plug>(vimtex-]])zz", "Tex Section Start", noremap = false},
		S = { "<cmd>let g:dirJumps='S'<cr><plug>(vimtex-][)zz", "Tex Section End", noremap = false},
		m = { "<cmd>let g:dirJumps='m'<cr><plug>(vimtex-]m)zz", "EnviroMent Start", noremap = false},
		M = { "<cmd>let g:dirJumps='M'<cr><plug>(vimtex-]M)zz", "EnviroMent End", noremap = false},
		n = { "<cmd>let g:dirJumps='n'<cr><plug>(vimtex-]n)zz", "Number Start", noremap = false},
		N = { "<cmd>let g:dirJumps='N'<cr><plug>(vimtex-]N)zz", "Number End", noremap = false},
		r = { "<cmd>let g:dirJumps='r'<cr><plug>(vimtex-]r)zz", "Frame Start", noremap = false},
		R = { "<cmd>let g:dirJumps='R'<cr><plug>(vimtex-]R)zz", "Frame End", noremap = false},
		C = { "<cmd>let g:dirJumps='C'<cr><plug>(vimtex-]/)zz", "Comment Start", noremap = false},
    },
}, {
	buffer = 0,
})

require("which-key").register({
	["<localleader>"] = {},
	["<leader>"] = {
		m = {
			m = { "<cmd>VimtexCompileSelected<cr>", "Build Selected" },
		},
	},
}, {
	buffer = 0,
	mode = "v",
})

if vim.api.nvim_get_var("dirJumps") == "f" then
   vim.api.nvim_set_var("dirJumps", "s")
end
if vim.api.nvim_get_var("panelRepeat") == "x" then
   vim.api.nvim_set_var("panelRepeat", "c")
end

vim.cmd([[let g:vimtex_index_split_width=60]])
vim.cmd([[let g:vimtex_quickfix_mode=0]])
vim.cmd([[let g:vimtex_automatic_xwin=0]])
vim.cmd([[let g:vimtex_view_method='zathura']])
vim.cmd([[let g:vimtex_view_forward_search_on_start=1]])
vim.cmd(
	[[let g:vimtex_compiler_latexmk = { 'build_dir' : '', 'callback' : 1, 'continuous' : 0, 'executable' : 'latexmk', 'hooks' : [], 'options' : [ '-verbose', '-file-line-error', '-synctex=1', '-interaction=nonstopmode', ] }]]
)
vim.cmd([[let g:vimtex_grammar_textidote={'jar':'~/.local/bin/textidote.jar', 'args':''}]])
vim.cmd([[set errorformat=]])
vim.cmd([[set errorformat+=%f(L%lC%c-L%\\d%\\+C%\\d%\\+):\ %m]])
vim.cmd([[set errorformat+=%-G%.%#]])

vim.cmd(
	[[let g:projectionist_heuristics={"OML-Thesis.tex":{ "*.tex":{"type":"chapMain","alternate":"OML-Thesis.tex"}, "Scripts/*.tex":{"type":"scripts","alternate":"OML-Thesis.tex"}, "1/*.tex":{"type":"chapOne","alternate":"IntroductiontoAM.tex"}, "2/*.tex":{"type":"chapTwo","alternate":"PowderBedFusion.tex"}, "3/*.tex":{"type":"chapThree","alternate":"ModellingofAM.tex"}, "4/*.tex":{"type":"chapFour","alternate":"PowderCharacterisation.tex"}, "5/*.tex":{"type":"chapFive","alternate":"PowderModel.tex"}, "6/*.tex":{"type":"chapSix","alternate":"MachineModel.tex"}, "7/*.tex":{"type":"chapSeven","alternate":"ProcessModel.tex"}, "Scripts/Packages.tex":{"type":"deps"}, "IntroductiontoAM.tex":{"type":"chapOneMain"}, "PowderBedFusion.tex":{"type":"chapTwoMain"}, "ModellingofAM.tex":{"type":"chapThreeMain"}, "PowderCharacterisation.tex":{"type":"chapFourMain"}, "PowderModel.tex":{"type":"chapFiveMain"}, "MachineModel.tex":{"type":"chapSixMain"}, "ProcessModel.tex":{"type":"chapSevenMain"}, "OML-Thesis.tex":{"type":"main"}, "Citations.bib":{"type":"citations"} }}]]
)
