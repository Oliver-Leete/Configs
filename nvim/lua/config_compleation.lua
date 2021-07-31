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

-- Compleation (compe) and Snippet (luasnip) Setup

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

-- Compe and luasnip mappings

vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", { expr = true })
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", { expr = true })
vim.api.nvim_set_keymap("i", "<cr>", [[compe#confirm(luaeval("require 'nvim-autopairs'.autopairs_cr()"))]], { expr = true })
vim.api.nvim_set_keymap("i", "<c-space>", "compe#complete()", { expr = true })
vim.api.nvim_set_keymap("i", "<C-d>", "compe#scroll({ 'delta': -4 })", { expr = true})
vim.api.nvim_set_keymap("i", "<C-f>", "compe#scroll({ 'delta': +4 })", { expr = true})
vim.api.nvim_set_keymap("i", "<C-e>", "compe#close('<C-e>')", { expr = true})

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
