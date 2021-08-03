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

-- Telescope Setup

local actions = require("telescope.actions")
local extensions = require("telescope").extensions
require("telescope").load_extension("bibtex")
require("telescope").load_extension("gh")
require("telescope").load_extension("media_files")
-- require("telescope").load_extension("session-lens")
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
        pickers = {
            buffers = {
                show_all_buffers = true,
                sort_mru = true,
            },
            grep_string = {
                use_regex = true,
            },
        },
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
