local dap_install = require("dap-install")
local dbg_list = require("dap-install.api.debuggers").get_installed_debuggers()

for _, debugger in ipairs(dbg_list) do
	dap_install.config(debugger)
end

require("nvim-dap-virtual-text").setup()
require("dapui").setup()

local dap = require("dap")

nnoremap("<leader>J", "<cmd>tabedit %<cr><cmd>lua require'dapui'.open()<cr>", "Open Debug Panels")
nnoremap("<leader>jo", "<cmd>tabedit %<cr><cmd>lua require'dapui'.open()<cr>", "Open Debug Panels")
nnoremap("<leader>jn", function() dap.step_over() end, "Step to the Next Line")
nnoremap("<leader>jN", function() dap.continue() end, "Step to the Next Breakpoint")
nnoremap("<leader>ji", function() dap.step_into() end, "Step In")
nnoremap("<leader>jo", function() dap.step_out() end, "Step Out")
nnoremap("<leader>jr", function() dap.step_back() end, "Reverse")
nnoremap("<leader>jc", function() dap.continue() end, "Run to Cursor")
nnoremap("<leader>jl", function() dap.repl.toggle() end, "Toggle REPL")
nnoremap("<leader>jq", function() dap.close() end, "Quit")
nnoremap("<leader>jb", function() dap.toggle_breakpoint() end, "Set Breakpoint")
nnoremap("<leader>je", function() require'dapui'.eval() end, "Eval Exression")
xnoremap("<leader>je", function() require'dapui'.eval() end, "Eval Exression")
-- nnoremap("<leader>jB", [["<cmd>lua require'dap'.toggle_breakpoint({" . input("Condition > ") . "})<cr>"]], "Set Conditional Breakpoint")
    -- mapxName.name("<leader>jr", "Breakpoints")
    -- nnoremap("<leader>jrr", "<cmd>silent !kittyPersistent debugterm juliadebug bp<cr>", "List Breakpoints")
    -- nnoremap("<leader>jrb", [["<cmd>silent !kittyPersistent debugterm juliadebug bp rm " . input("Point to Remove > ") . "<cr>"]], "Remove Breapoint")
    -- nnoremap("<leader>jrw", [["<cmd>silent !kittyPersistent debugterm juliadebug w rm " . input("Item to Remove > ") . "<cr>"]], "Remove Watchlist")
