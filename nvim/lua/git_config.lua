require("gitsigns").setup({
	sign_priority = 6,
})
-- DiffView.nvim

local cb = require("diffview.config").diffview_callback
require("diffview").setup({
	diff_binaries = false,
	use_icons = true,
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

-- conflict.nvim
require("git-conflict").setup({
	default_mappings = false,
	disable_diagnostics = false,
	highlights = {
		incoming = "DiffText",
		current = "DiffAdd",
	},
})

vim.api.nvim_create_autocmd("User", {
	pattern = "GitConflictDetected",
	callback = function()
		if vim.b[0].localCommands then
			table.insert(vim.b[0].localCommands, {
				source = "conflict",
				name = "List conflicts",
				command = "GitConflictListQf | Telescope quickfix theme=get_ivy",
			})
		else
			vim.b[0].localCommands = {
				{
					source = "conflict",
					name = "List conflicts",
					command = "GitConflictListQf | Telescope quickfix theme=get_ivy",
				},
			}
		end
        vim.keymap.set('n', 'co', '<Plug>(git-conflict-ours)')
        vim.keymap.set('n', 'cb', '<Plug>(git-conflict-both)')
        vim.keymap.set('n', 'c0', '<Plug>(git-conflict-none)')
        vim.keymap.set('n', 'ct', '<Plug>(git-conflict-theirs)')
        vim.keymap.set('n', '[x', function() markGoCentre("GitConflictPrevConflict", "x")end)
        vim.keymap.set('n', ']x', function() markGoCentre("GitConflictNextConflict", "x")end)
	end,
})

-- vim.api.nvim_create_autocmd("User", {
-- 	pattern = "GitConflictDetected",
-- 	callback = function()
-- 		Notification_Dict["conflict"] = vim.notify(
-- 			"Conflict detected",
-- 			"error",
-- 			{ title = "Git", replace = Notification_Dict["conflict"] }
-- 		)
-- 	end,
-- })
