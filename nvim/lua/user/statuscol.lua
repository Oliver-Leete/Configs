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

-- ScGs = function()
--     local sign = vim.fn.sign_getplaced(0, { lnum = vim.v.lnum, group = "gitsigns_vimfn_signs_" })[1].signs
--     local hl = "@comment"
--     if #sign >= 1 and sign[1].name then
--         hl = sign[1].name
--     end
--     return "%#" .. hl .. "#" .. "🭰"
-- end

vim.o.statuscolumn = "%=%l%@v:lua.ScSa@%s%T"
