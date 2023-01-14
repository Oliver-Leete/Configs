local builtin = require("statuscol.builtin")

local function diagnostic_click(args)
	if args.button == "l" then
		vim.diagnostic.open_float({ border = Border, scope = "line", source = "always" })
	elseif args.button == "m" then
		vim.lsp.buf.code_action()
	end
end

require("statuscol").setup({
  separator = "",
  setopt = true,
  order = "NSFs",
  -- Click actions
  Lnum                   = builtin.lnum_click,
  FoldPlus               = builtin.foldplus_click,
  FoldMinus              = builtin.foldminus_click,
  FoldEmpty              = builtin.foldempty_click,
  DapBreakpointRejected  = builtin.toggle_breakpoint,
  DapBreakpoint          = builtin.toggle_breakpoint,
  DapBreakpointCondition = builtin.toggle_breakpoint,
  DiagnosticSignError    = diagnostic_click,
  DiagnosticSignHint     = diagnostic_click,
  DiagnosticSignInfo     = diagnostic_click,
  DiagnosticSignWarn     = diagnostic_click,
  GitSignsTopdelete      = builtin.gitsigns_click,
  GitSignsUntracked      = builtin.gitsigns_click,
  GitSignsAdd            = builtin.gitsigns_click,
  GitSignsChangedelete   = builtin.gitsigns_click,
  GitSignsDelete         = builtin.gitsigns_click,
})
