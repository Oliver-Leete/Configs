----------------------------------------------------------------------------------------------------
--
--                      _   _   ______    ____   __      __  _____   __  __                       --
--                     | \ | | |  ____|  / __ \  \ \    / / |_   _| |  \/  |                      --
--                     |  \| | | |__    | |  | |  \ \  / /    | |   | \  / |                      --
--                     | . ` | |  __|   | |  | |   \ \/ /     | |   | |\/| |                      --
--                     | |\  | | |____  | |__| |    \  /     _| |_  | |  | |                      --
--                     |_| \_| |______|  \____/      \/     |_____| |_|  |_|                      --
--                                                                                                --
----------------------------------------------------------------------------------------------------
-- Oliver Leete <oliverleete@gmail.com>                                                            --
-- https://github.com/oliver-leete                                                                 --
----------------------------------------------------------------------------------------------------

-- Disable builtins
local disabled_built_ins = {
	"netrw",
	"netrwPlugin",
	"netrwSettings",
	"netrwFileHandlers",
	"gzip",
	"zip",
	"zipPlugin",
	"tar",
	"tarPlugin",
	"getscript",
	"getscriptPlugin",
	"vimball",
	"vimballPlugin",
	"2html_plugin",
	"logipat",
	"rrhelper",
	"spellfile_plugin",
	"matchit",
}

for _, plugin in pairs(disabled_built_ins) do
	vim.g["loaded_" .. plugin] = 1
end

-- Auto Sessions

require("auto-session").setup({
	log_level = "info",
	auto_session_enable_last_session = false,
	auto_session_enabled = true,
	auto_save_enabled = nil,
	auto_restore_enabled = nil,
	auto_session_suppress_dirs = nil,
})

-- Autosave Setup

-- require("autosave").setup({
--     verbosity = 0,
--     enabled = true,
--     execution_message = "AutoSave: saved at " .. vim.fn.strftime("%H:%M:%S"),
--     events = {"InsertLeave", "TextChanged"},
--     conditions = {
--         exists = true,
--         filetype_is_not = {},
--         modifiable = true
--     },
--     write_all_buffers = true,
--     on_off_commands = true,
--     clean_command_line_interval = 2500
-- })

-- DiffView.nvim

local cb = require("diffview.config").diffview_callback
require("diffview").setup({
	diff_binaries = false, -- Show diffs for binaries
	file_panel = {
		width = 35,
		use_icons = true, -- Requires nvim-web-devicons
	},
	key_bindings = {
		-- The `view` bindings are active in the diff buffers, only when the current
		-- tabpage is a Diffview.
		view = {
			["<tab>"] = cb("select_next_entry"), -- Open the diff for the next file
			["<s-tab>"] = cb("select_prev_entry"), -- Open the diff for the previous file
			["<leader>x"] = cb("focus_files"), -- Bring focus to the files panel
			["<esc>"] = cb("focus_files"),
			-- ["<leader>b"] = cb("toggle_files"),       -- Toggle the files panel.
		},
		file_panel = {
			["j"] = cb("next_entry"), -- Bring the cursor to the next file entry
			["<down>"] = cb("next_entry"),
			["k"] = cb("prev_entry"), -- Bring the cursor to the previous file entry.
			["<up>"] = cb("prev_entry"),
			["<cr>"] = cb("select_entry"), -- Open the diff for the selected entry.
			["o"] = "<cmd>lua require('diffview').on_keypress('select_entry')<cr><cmd>sleep 100m<cr><cmd>DiffviewToggleFiles<cr>",
			["p"] = "<cmd>lua require('diffview').on_keypress('select_entry')<cr><cmd>DiffviewFocusFiles<cr>",
			["R"] = cb("refresh_files"), -- Update stats and entries in the file list.
			["<tab>"] = cb("select_next_entry"),
			["<s-tab>"] = cb("select_prev_entry"),
			["<leader>t"] = cb("focus_files"),
			["<leader>b"] = cb("toggle_files"),
			["<esc>"] = "<cmd>tabclose<cr>",
		},
	},
})

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
-- Compleation Setup
require("luasnip/loaders/from_vscode").load({
	paths = { "/home/oleete/.config/nvim/snippets", "/home/oleete/.config/nvim/pluged/friendly-snippets/snippets" },
})
vim.o.completeopt = "menuone,noselect"
require("compe").setup({
	enabled = true,
	autocomplete = true,
	debug = false,
	min_length = 1,
	preselect = "enable",
	throttle_time = 80,
	source_timeout = 200,
	incomplete_delay = 400,
	max_abbr_width = 100,
	max_kind_width = 100,
	max_menu_width = 100,
	documentation = {
		border = { "", "", "", " ", "", "", "", " " },
		winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
		max_width = 120,
		min_width = 60,
		max_height = math.floor(vim.o.lines * 0.3),
		min_height = 1,
	},

	source = {
		path = true,
		buffer = true,
		calc = true,
		nvim_lsp = true,
		nvim_lua = true,
		nvim_treesitter = false,
		vsnip = false,
		luasnip = true,
		omni = {
			filetypes = { "tex" },
		},
		tabnine = true,
	},
})

local t = function(str)
	return vim.api.nvim_replace_termcodes(str, true, true, true)
end
local check_back_space = function()
	local col = vim.fn.col(".") - 1
	if col == 0 or vim.fn.getline("."):sub(col, col):match("%s") then
		return true
	else
		return false
	end
end
local luasnip = require("luasnip")
_G.tab_complete = function()
	-- if vim.fn.pumvisible() == 1 then
	--   return t '<C-n>'
	if luasnip.expand_or_jumpable() then
		return t("<Plug>luasnip-expand-or-jump")
	else
		check_back_space()
		return t("<cmd>Tabout<cr>")
	end
end

_G.s_tab_complete = function()
	-- if vim.fn.pumvisible() == 1 then
	--   return t '<C-p>'
	if luasnip.jumpable(-1) then
		return t("<Plug>luasnip-jump-prev")
	else
		return t("<cmd>TaboutBack<cr>")
	end
end

-- Map tab to the above tab complete functions
vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", { expr = true })
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", { expr = true })
vim.api.nvim_set_keymap("i", "<cr>", 'compe#confirm("<cr>")', { expr = true })
vim.api.nvim_set_keymap("i", "<c-space>", "compe#complete()", { expr = true })

-- AutoPairs Setup

require("nvim-autopairs").setup({
	check_ts = true,
	map_cr = true, --  map <CR> on insert mode
	map_complete = true, -- it will auto insert `(` after select function or method item
	enable_check_bracket_line = true,
})

vim.g.diagnostic_auto_popup_while_jump = 0
vim.g.diagnostic_enable_virtual_text = 0
vim.g.diagnostic_enable_underline = 0
vim.g.completion_timer_cycle = 200

-- Telescope Setup

local actions = require("telescope.actions")
local extensions = require("telescope").extensions
require("telescope").load_extension("bibtex")
require("telescope").load_extension("gh")
require("telescope").load_extension("media_files")
require("telescope").load_extension("session-lens")
require("telescope").load_extension("heading")
local trouble = require("trouble.providers.telescope")

require("telescope").setup({
	defaults = {
		vimgrep_arguments = {
			"rg",
			"--color=never",
			"--no-heading",
			"--with-filename",
			"--line-number",
			"--column",
			"--smart-case",
		},
		prompt_prefix = "> ",
		selection_caret = "> ",
		entry_prefix = "  ",
		initial_mode = "insert",
		selection_strategy = "reset",
		sorting_strategy = "descending",
		layout_strategy = "vertical",
		layout_config = {
			vertical = {
				width = 100,
				preview_height = 40,
				height = 80,
				mirror = false,
			},
		},
		file_sorter = require("telescope.sorters").get_fuzzy_file,
		file_ignore_patterns = {},
		generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
		path_display = { "shorten" },
		winblend = 0,
		border = {},
		borderchars = {
			{ "─", "│", "─", "│", "┌", "┐", "┘", "└" },
			results = { "─", "│", " ", "│", "┌", "┐", "│", "│" },
			prompt = { "─", "│", "─", "│", "├", "┤", "┘", "└" },
			preview = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
		},
		color_devicons = true,
		use_less = true,
		set_env = { ["COLORTERM"] = "truecolor" }, -- default = nil,
		file_previewer = require("telescope.previewers").vim_buffer_cat.new,
		grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
		qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,

		mappings = {
			i = {
				["<C-q>"] = actions.smart_send_to_qflist,
				["<C-Q>"] = actions.smart_add_to_qflist,
				["<C-l>"] = actions.smart_send_to_loclist,
				["<C-L>"] = actions.smart_add_to_loclist,
				["<c-e>"] = trouble.open_with_trouble,
				["<c-E>"] = trouble.open_selected_with_trouble,
				["<C-n>"] = actions.cycle_history_next,
				["<C-p>"] = actions.cycle_history_prev,
				["<C-space>"] = actions.toggle_selection + actions.move_selection_worse,
				["<C-j>"] = actions.move_to_top,
				["<C-h>"] = actions.move_to_middle,
				["<C-k>"] = actions.move_to_bottom,
				["<C-s>"] = extensions.hop.hop,
				["<C-S>"] = extensions.hop.hop_toggle_selection,
			},
			n = {
				["<C-q>"] = actions.smart_send_to_qflist,
				["<C-Q>"] = actions.smart_add_to_qflist,
				["<C-l>"] = actions.smart_send_to_loclist,
				["<C-L>"] = actions.smart_add_to_loclist,
				["<c-e>"] = trouble.open_with_trouble,
				["<c-E>"] = trouble.open_selected_with_trouble,
				["<C-n>"] = actions.cycle_history_next,
				["<C-p>"] = actions.cycle_history_prev,
				["<C-space>"] = actions.toggle_selection + actions.move_selection_worse,
				["<C-j>"] = actions.move_to_top,
				["<C-h>"] = actions.move_to_middle,
				["<C-k>"] = actions.move_to_bottom,
				["<C-s>"] = extensions.hop.hop,
				["<C-S>"] = extensions.hop.hop_toggle_selection,
			},
		},
	},
	extensions = {
		bibtex = {
			depth = 2,
			global_files = { "/home/oleete/UniDrive/1_Thesis/0.1_LaTeX/Citations.bib" },
		},
		fzf = {
			override_generic_sorter = true, -- override the generic sorter
			override_file_sorter = true, -- override the file sorter
			case_mode = "smart_case", -- or "ignore_case" or "respect_case"
		},
		hop = {
			keys = { "t", "n", "s", "e", "r", "i", "a", "o", "d", "h", "g", "j", "p", "l", "f", "u", "w", "y" },
			sign_hl = { "WarningMsg", "Title" },
			line_hl = { "CursorLine", "Noraml" },
			clear_selection_hl = true,
			trace_entry = true,
			reset_selection = true,
		},
		media_files = {},
	},
})

require("telescope").load_extension("hop")

local action_state = require("telescope.actions.state")

local open_dif = function()
	local selected_entry = action_state.get_selected_entry()
	local value = selected_entry["value"]
	-- close Telescope window properly prior to switching windows
	vim.api.nvim_win_close(0, true)
	local cmd = "DiffviewOpen " .. value
	vim.cmd(cmd)
end
local open_dif_mergebase = function()
	local selected_entry = action_state.get_selected_entry()
	local value = selected_entry["value"]
	-- close Telescope window properly prior to switching windows
	vim.api.nvim_win_close(0, true)
	local cmd = "DiffviewOpen ..." .. value
	vim.cmd(cmd)
end
local open_single_dif = function()
	local selected_entry = action_state.get_selected_entry()
	local value = selected_entry["value"]
	-- close Telescope window properly prior to switching windows
	vim.api.nvim_win_close(0, true)
	local cmd = "DiffviewOpen " .. value .. "~1.." .. value
	vim.cmd(cmd)
end
local change_gitsign_base = function()
	local selected_entry = action_state.get_selected_entry()
	local value = selected_entry["value"]
	vim.api.nvim_win_close(0, true)
	local cmd = "Gitsigns change_base " .. value
	vim.cmd(cmd)
end

function _G.gitsign_change_base()
	require("telescope.builtin").git_commits({
		attach_mappings = function(_, map)
			map("n", "<cr>", change_gitsign_base)
			map("i", "<cr>", change_gitsign_base)
			return true
		end,
	})
end
function _G.gitsign_bchange_base()
	require("telescope.builtin").git_bcommits({
		attach_mappings = function(_, map)
			map("n", "<cr>", change_gitsign_base)
			map("i", "<cr>", change_gitsign_base)
			return true
		end,
	})
end

function _G.git_commits_againsthead()
	require("telescope.builtin").git_commits({
		attach_mappings = function(_, map)
			map("n", "<cr>", open_dif)
			map("i", "<cr>", open_dif)
			return true
		end,
	})
end
function _G.git_commits_onechange()
	require("telescope.builtin").git_commits({
		attach_mappings = function(_, map)
			map("n", "<cr>", open_single_dif)
			map("i", "<cr>", open_single_dif)
			return true
		end,
	})
end

function _G.git_branch_dif()
	require("telescope.builtin").git_branches({
		attach_mappings = function(_, map)
			map("n", "<cr>", open_dif)
			map("i", "<cr>", open_dif)
			return true
		end,
	})
end
function _G.git_branch_mergebase()
	require("telescope.builtin").git_branches({
		attach_mappings = function(_, map)
			map("n", "<cr>", open_dif_mergebase)
			map("i", "<cr>", open_dif_mergebase)
			return true
		end,
	})
end

require("telescope").load_extension("fzf")
require("telescope").load_extension("bibtex")

require("todo-comments").setup({
	signs = true,
	keywords = {
		FIX = { icon = " ", color = "error", alt = { "FIXME", "BUG", "FIXIT", "FIX", "ISSUE" } },
		TODO = { icon = " ", color = "info" },
		HACK = { icon = " ", color = "warning", alt = { "JANK", "WORKAROUND" } },
		WARN = { icon = " ", color = "warning", alt = { "WARNING" } },
		PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
		NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
	},
	highlight = {
		before = "fg",
		keyword = "wide",
		after = "fg",
	},
	colors = {
		error = { "LspDiagnosticsDefaultError", "ErrorMsg", "#DC2626" },
		warning = { "LspDiagnosticsDefaultWarning", "WarningMsg", "#FBBF24" },
		info = { "LspDiagnosticsDefaultInformation", "#2563EB" },
		hint = { "LspDiagnosticsDefaultHint", "#10B981" },
		default = { "Identifier", "#7C3AED" },
	},
})

require("surround").setup({})

require("hop").setup({ keys = "tnseriaodhgjplfuwybkvmcxzq" })
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
require("numb").setup()
require("foldsigns").setup()
require("range-highlight").setup()
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
