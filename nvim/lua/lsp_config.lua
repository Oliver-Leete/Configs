local nvim_lsp = require("lspconfig")

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

vim.cmd([[
    sign define DiagnosticSignError text= texthl=DiagnosticSignError linehl= numhl=DiagnosticSignError
    sign define DiagnosticSignWarn text= texthl=DiagnosticSignWarn linehl= numhl=DiagnosticSignWarn
    sign define DiagnosticSignInfo text= texthl=DiagnosticSignInfo linehl= numhl=DiagnosticSignInfo
    sign define DiagnosticSignHint text= texthl=DiagnosticSignHint linehl= numhl=DiagnosticSignHint
]])

vim.diagnostic.config({
	underline = false,
	virtual_text = { severity = "Error" },
	signs = true,
	update_in_insert = false,
	severity_sort = true,
})
Notification_Dict = {}

local custom_attach = function(client)
	if client.name ~= "null-ls" then
		Notification_Dict[client.name] = pcall(
			vim.notify(client.name .. " started", "info", { title = "LSP", replace = Notification_Dict[client.name] })
		)
	end

	-- LSP Binding Override
	vim.keymap.set("n", "KK", function()
		vim.lsp.buf.hover({ focusable = false })
	end)
	vim.keymap.set("n", "gd", "<cmd>Telescope lsp_definitions theme=get_ivy<cr>")

	vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
		border = "single",
		focusable = false,
	})
	vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
		border = "single",
		focusable = false,
	})

	require("lsp_signature").on_attach({
		bind = true,
		doc_lines = 10,
		floating_window = true,
		fixpos = true,
		hint_enable = false,
		use_lspsaga = false,
		hi_parameter = "IncSearch",
		max_height = 12,
		max_width = 120,
		extra_trigger_chars = { ";" },
		handler_opts = {
			border = "single",
		},
	})
end

require("grammar-guard").init()
require("lspconfig").grammar_guard.setup({
	cmd = { "/home/oleete/.local/share/nvim/lsp_servers/ltex/ltex-ls/bin/ltex-ls" },
	settings = {
		ltex = {
			enabled = { "latex", "tex", "bib", "markdown" },
			checkFrequency = "save",
			language = "en-GB",
			setenceCacheSize = 2000,
			diagnosticSeverity = { MORFOLOGIK_RULE_EN_GB = "hint", default = "info" },
			additionalRules = {
				enablePickyRules = false,
				motherTongue = "en-GB",
			},
			latex = {
				environments = { Fortran = "ignore", jllisting = "ignore", algorithmic = "ignore" },
				commands = {
					["\\twosubfigures{}{}{}{}{}{}"] = "ignore",
					["\\threesubfigures{}{}{}{}{}{}{}{}{}"] = "ignore",
					["\\subfile{}"] = "ignore",
					["\\glsname{}"] = "dummy",
					["\\gls{}"] = "dummy",
					["\\glsfirst{}"] = "dummy",
					["\\pgls{}"] = "dummy",
					["\\ac{}"] = "dummy",
					["\\acl{}"] = "dummy",
					["\\acs{}"] = "dummy",
					["\\acf{}"] = "dummy",
					["\\pac{}"] = "dummy",
					["\\Pac{}"] = "dummy",
				},
			},
			dictionary = { ["en-GB"] = { "ANSYS", "UPF" } },
			disabledRules = { ["en-GB"] = { "OXFORD_SPELLING_Z_NOT_S" } },
			hiddenFalsePositives = {},
		},
	},
})

require("nvim-lsp-installer").setup({})

local lspconfig = require("lspconfig")
local default = {
	on_attach = custom_attach,
	capabilities = capabilities,
	flags = { debounce_text_changes = 500 },
}

lspconfig.bashls.setup(default)
lspconfig.ccls.setup(default)
lspconfig.fortls.setup(default)
lspconfig.julials.setup(default)
lspconfig.pyright.setup(default)

lspconfig.sumneko_lua.setup({
	on_attach = custom_attach,
	capabilities = capabilities,
	flags = { debounce_text_changes = 500 },
	root_dir = nvim_lsp.util.root_pattern("init.lua"),
	settings = {
		Lua = {
			runtime = {
				version = "LuaJIT",
				path = vim.split(package.path, ";"),
			},
			diagnostics = {
				-- enable = false,
				globals = { "vim" },
			},
			workspace = {
				library = {
					[vim.fn.expand("$VIMRUNTIME/lua")] = true,
					[vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
				},
				maxPreload = 3000,
				preloadFileSize = 500,
			},
			telemetry = {
				enable = false,
			},
		},
	},
})

lspconfig.texlab.setup({
	on_attach = custom_attach,
	capabilities = capabilities,
	flags = { debounce_text_changes = 500 },
	root_dir = nvim_lsp.util.root_pattern(".git"),
	settings = {
		texlab = {
			build = {
				args = { "-pdf", "-interaction=nonstopmode", "-synctex=1", "%f" },
				executable = "latexmk",
				onSave = false,
				forwardSearchAfter = true,
			},
			forwardSearch = {
				executable = "zathura",
				args = { "--synctex-forward", "%l:1:%f", "%p" },
				onSave = false,
			},
			chktex = {
				onEdit = false,
				onOpenAndSave = false,
			},
		},
	},
})
lspconfig.hls.setup({
	on_attach = custom_attach,
	capabilities = capabilities,
	flags = { debounce_text_changes = 500 },
	filetypes = { "haskell", "lhaskell" },
	root_dir = nvim_lsp.util.root_pattern("*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml", ".git"),
})

require("rust-tools").setup({
	server = {
		on_attach = custom_attach,
		capabilities = capabilities,
		flags = { debounce_text_changes = 501 },
		standalone = true,
	},
	tools = {
		executor = require("rust-tools/executors").kitty,
	},
	dap = {
		adapter = require("rust-tools.dap").get_codelldb_adapter(
			"/home/oleete/.local/share/nvim/dapinstall/codelldb/extension/adapter/codelldb",
			"/home/oleete/.local/share/nvim/dapinstall/codelldb/extension/lldb/lib/liblldb.so"
		),
	},
})

-- Null LS
require("null-ls").setup({
	on_attach = custom_attach,
	diagnostics_format = "[#{c}] #{m} (#{s})",
	sources = {
		require("null-ls").builtins.code_actions.gitsigns.with({ filetypes = { "kitty" } }),
		require("null-ls").builtins.formatting.trim_whitespace,
		require("null-ls").builtins.formatting.trim_newlines,
		require("null-ls").builtins.formatting.shfmt,
		require("null-ls").builtins.formatting.shellharden,
		require("null-ls").builtins.formatting.stylua,
		require("null-ls").builtins.formatting.fish_indent,
		require("null-ls").builtins.formatting.latexindent,
		require("null-ls").builtins.diagnostics.markdownlint,
		require("null-ls").builtins.diagnostics.chktex,
		require("null-ls").builtins.hover.dictionary.with({ filetypes = { "tex", "markdown" } }),
		require("null-ls").builtins.code_actions.refactoring,
	},
})
