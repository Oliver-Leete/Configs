vim.api.nvim_set_option("errorformat", [[%tRROR:\ %m\ at\ %f:%l,%-G%.%#]])
vim.api.nvim_set_option("makeprg", [[julia\ -e\ \'using\ Pkg;\ Pkg.precompile()\']])
vim.api.nvim_buf_set_option(0, "commentstring", [[#%s]])

local juliaREPL = require("toggleterm.terminal").Terminal:new({
	cmd = "julia",
	count = 3,
})

function _juliaREPL_toggle()
	juliaREPL:toggle()
end

require("which-key").register({
	["<leader>"] = {
		["/"] = {
			m = { [["<cmd>Esource " . split(getcwd(), '/')[-1] . "<cr>"]], "Project Main File", expr = true },
			v = {
				m = { [["<cmd>Esource " . split(getcwd(), '/')[-1] . "<cr>"]], "Project Main File", expr = true },
			},
			x = {
				m = { [["<cmd>Ssource " . split(getcwd(), '/')[-1] . "<cr>"]], "Project Main File", expr = true },
			},
			T = {
				m = { [["<cmd>Ssource " . split(getcwd(), '/')[-1] . "<cr>"]], "Project Main File", expr = true },
			},
			n = {
				m = { [["<cmd>Esource " . split(getcwd(), '/')[-1] . "<cr>"]], "Project Main File", expr = true },
			},
		},
		v = {
			i = { "<cmd>lua _juliaREPL_toggle()<cr>", "Julia Terminal" },
		},
		i = {
			i = { "<cmd>lua _juliaREPL_toggle()<cr>", "Julia Terminal" },
		},
		r = {
			d = {
				[[[fyyO<esc><cmd>.!cat ~/.config/nvim/filetype/julia/func_docstring.txt<cr>pdw>>/TODO:<cr>]],
				"Make Docstring",
				noremap = false,
			},
		},
		m = {
			m = { [[<cmd>2TermExec cmd="~/.config/nvim/filetype/julia/precompile"<cr>]], "Precompile" },
			t = { [[<cmd>2TermExec cmd="~/.config/nvim/filetype/julia/test"<cr>]], "Test Package" },
			c = { [[<cmd>2TermExec cmd="~/.config/nvim/filetype/julia/testCov"<cr>]], "Coverage Check Package" },
			b = { [[<cmd>2TermExec cmd="~/.config/nvim/filetype/julia/benchmark"<cr>]], "Benckmark Package" },
			d = { [[<cmd>2TermExec cmd="~/.config/nvim/filetype/julia/docBuild"<cr>]], "Build Package Documentation" },
		},
	},
}, {
	buffer = 0,
})

vim.cmd(
	[[let g:projectionist_heuristics={"src/*.jl":{"src/*.jl":{"type":"source","alternate":"test/{}_tests.jl","related":["benckmark/{}_benchmarks.jl","test/{}_tests.jl","docs/src/{}.md"]},"benchmark/*_benchmarks.jl":{"type":"bench","alternate":"src/{}.jl","related":["src/{}.jl","test/{}_tests.jl","docs/src/{}.md"]},"test/*_tests.jl":{"type":"test","alternate":"src/{}.jl","related":["benckmark/{}_benchmarks.jl","src/{}.jl","docs/src/{}.md"]},"docs/src/*.md":{"type":"doc","alternate":"src/{}.jl","related":["benckmark/{}_benchmarks.jl","test/{}_tests.jl","src/{}.jl"]},"README.md":{"type":"readme"},"Project.toml":{"type":"deps"},}}]]
)
