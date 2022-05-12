require("gitsigns").setup({
	sign_priority = 6,
})
-- DiffView.nvim

local cb = require("diffview.config").diffview_callback
require("diffview").setup({
	diff_binaries = false,
	use_icons = true,
	file_panel = {
		width = 35,
	},
	key_bindings = {
		view = {
			["<tab>"] = cb("select_next_entry"),
			["<s-tab>"] = cb("select_prev_entry"),
			["<leader>x"] = cb("focus_files"),
			["<esc>"] = cb("focus_files"),
		},
		file_panel = {
			["j"] = cb("next_entry"),
			["<down>"] = cb("next_entry"),
			["k"] = cb("prev_entry"),
			["<up>"] = cb("prev_entry"),
			["<cr>"] = cb("select_entry"),
			["o"] = cb("select_entry") .. "<cmd>sleep 100m<cr><cmd>DiffviewToggleFiles<cr>",
			["p"] = cb("select_entry") .. "<cmd>DiffviewFocusFiles<cr>",
			["<2-LeftMouse>"] = cb("select_entry"),
			["-"] = cb("toggle_stage_entry"),
			["S"] = cb("stage_all"),
			["U"] = cb("unstage_all"),
			["XX"] = cb("restore_entry"),
			["R"] = cb("refresh_files"),
			["<s-tab>"] = cb("select_prev_entry"),
			["<leader>t"] = cb("focus_files"),
			["<leader>x"] = cb("toggle_files"),
			["<esc>"] = "<cmd>DiffviewClose<cr>",
		},
	},
})
