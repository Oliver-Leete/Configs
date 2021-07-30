-- Treesitter

require("nvim-treesitter.configs").setup({
	autopairs = { enable = true },
	highlight = { enable = true },
	indent = { enable = true },
	rainbow = { enable = true },
	matchup = { enable = true },
	textobjects = {
		select = {
			enable = true,
			lookahead = true,
			keymaps = {
				["a,"] = "@parameter.outer",
				["i,"] = "@parameter.inner",
				["ao"] = "@class.outer",
				["io"] = "@class.inner",
				["af"] = "@function.outer",
				["if"] = "@function.inner",
				["aF"] = "@call.outer",
				["iF"] = "@call.inner",
				["ac"] = "@conditional.outer",
				["ic"] = "@conditional.inner",
				["aC"] = "@comment.outer",
				["aL"] = "@loop.outer",
				["il"] = "@loop.inner",
				["aB"] = "@block.outer",
				["iB"] = "@block.inner",
			},
		},
		move = {
			enable = true,
			set_jumps = true,
		},
		lsp_interop = {
			enable = true,
			border = "single",
			peek_definition_code = {
				["<leader>pf"] = "@function.outer",
				["<leader>po"] = "@class.outer",
			},
		},
		swap = {
			enable = true,
		},
	},
	refactor = {
		highlight_current_scope = { enable = false },
		highlight_definitions = { enable = false },
		smart_rename = {
			enable = true,
			keymaps = {
				smart_rename = "<leader>rt",
			},
		},
		navigation = {
			enable = true,
			keymaps = {
				list_definitions_toc = "<nop>",
				goto_definitions = "<nop>",
				list_definitions = "<nop>",
				goto_next_usage = "]#",
				goto_previous_usage = "[#",
			},
		},
	},
	playground = {
		enable = true,
		disable = {},
		updatetime = 25,
		persist_queries = false,
		keybindings = {
			toggle_query_editor = "o",
			toggle_hl_groups = "i",
			toggle_injected_languages = "t",
			toggle_anonymous_nodes = "a",
			toggle_language_display = "I",
			focus_language = "f",
			unfocus_language = "F",
			update = "R",
			goto_node = "<cr>",
			show_help = "?",
		},
	},
	query_linter = {
		enable = true,
		use_virtual_text = true,
		lint_events = { "BufWrite", "CursorHold" },
	},
	textsubjects = {
		enable = true,
		keymaps = {
			[";"] = "textsubjects-smart",
		},
	},
})

require("nvim-biscuits").setup({
	on_events = { "InsertLeave", "CursorHoldI" },
	default_config = {
		min_distance = 10,
	},
	language_config = {
		haskell = {
			disabled = true,
		},
	},
})

require("tabout").setup({
	tabkey = "<c-`>",
	act_as_tab = false,
	completion = false,
    enable_backwards = true,
	tabouts = {
		{ open = "'", close = "'" },
		{ open = '"', close = '"' },
		{ open = "`", close = "`" },
		{ open = "(", close = ")" },
		{ open = "[", close = "]" },
		{ open = "{", close = "}" },
		{ open = "<", close = ">" },
	},
	ignore_beginning = false,
	exclude = {},
})

require("iswap").setup({ keys = "tnseriaodhgjplfuwybkvmcxzq" })
require("tsht").config.hint_keys = {
	"t",
	"n",
	"s",
	"e",
	"r",
	"i",
	"a",
	"o",
	"d",
	"h",
	"g",
	"j",
	"p",
	"l",
	"f",
	"u",
	"w",
	"y",
}
