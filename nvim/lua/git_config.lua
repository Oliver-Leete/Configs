require("gitsigns").setup({
	sign_priority = 6,
})
-- DiffView.nvim

local actions = require("diffview.config").actions
require("diffview").setup({
	diff_binaries = false,
	use_icons = true,
	key_bindings = {
		view = {
			["<esc>"] = actions.focus_files,
		},
        file_panel = {
                ["<esc>"] = function() vim.cmd("DiffviewClose") end,
        },
        file_history_panel = {
                ["<esc>"] = function() vim.cmd("DiffviewClose") end,
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
		vim.keymap.set("n", "co", "<Plug>(git-conflict-ours)")
		vim.keymap.set("n", "cb", "<Plug>(git-conflict-both)")
		vim.keymap.set("n", "c0", "<Plug>(git-conflict-none)")
		vim.keymap.set("n", "ct", "<Plug>(git-conflict-theirs)")
		vim.keymap.set("n", "[x", function()
			markGoCentre("GitConflictPrevConflict", "x")
		end)
		vim.keymap.set("n", "]x", function()
			markGoCentre("GitConflictNextConflict", "x")
		end)
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
