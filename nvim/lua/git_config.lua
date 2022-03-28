----------------------------------------------------------------------------------------------------
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

-- Git Signs Settup

require("gitsigns").setup({
	signs = {
		add = { hl = "GitSignsAdd", numhl = "GitSignsAdd", linehl = "GitSignsAdd", text = "▋" },
		change = { hl = "GitSignsChange", numhl = "GitSignsChange", linehl = "GitSignsChange", text = "▋" },
		delete = { hl = "GitSignsDelete", numhl = "GitSignsDelete", linehl = "GitSignsDelete", text = "▂" },
		topdelete = { hl = "GitSignsDelete", numhl = "GitSignsDelete", linehl = "GitSignsDelete", text = "▔" },
		changedelete = { hl = "GitSignsDelete", numhl = "GitSignsDelete", linehl = "GitSignsDelete", text = "▋" },
		empty = {}, -- Unused

		base = nil, -- Use index
		signcolumn = true,
		numhl = true,
		linehl = false,
	},
	--     signs_sec = {
	--         add          = {hl = 'GitSignsAdd'   , numhl='GitSignsAdd'   , linehl='GitSignsAdd'   , text = '▎' },
	--         change       = {hl = 'GitSignsChange', numhl='GitSignsChange', linehl='GitSignsChange', text = '▎' },
	--         delete       = {hl = 'GitSignsDelete', numhl='GitSignsDelete', linehl='GitSignsDelete', text = '_' },
	--         topdelete    = {hl = 'GitSignsDelete', numhl='GitSignsDelete', linehl='GitSignsDelete', text = '‾' },
	--         changedelete = {hl = 'GitSignsDelete', numhl='GitSignsDelete', linehl='GitSignsDelete', text = '▎' },
	--         empty        = {},

	--         base       = nil,
	--         signcolumn = true,
	--         numhl      = true,
	--         linehl     = false,
	--     },
	keymaps = {},
	numhl = true,
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
