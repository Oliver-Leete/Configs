local dap = require("dap")

vim.api.nvim_set_hl(0, "CursorLineError", { fg = "#E82424", bg = "#363646" })
vim.cmd([[
    sign define DapBreakpoint text= texthl=DiagnosticSignError linehl= numhl= culhl=CursorLineError
    sign define DapBreakpointCondition text= texthl=DiagnosticSignError linehl= numhl= culhl=CursorLineError
    sign define DapLogPoint text= texthl=DiagnosticSignWarn linehl= numhl= culhl=CursorLineWarn
    sign define DapStopped text= texthl=DiagnosticSignError linehl= numhl= culhl=CursorLineError
    sign define DapBreakpointRejected text= texthl=DiagnosticSignInfo linehl= numhl= culhl=CursorLineInfo
]])

require("nvim-dap-virtual-text").setup()
require("dapui").setup({
    controls = {
        enabled = false,
        icons = {
            pause = "",
            play = "",
            step_into = "",
            step_over = "",
            step_out = "",
            step_back = "",
            run_last = "↻",
            terminate = "栗",
        },
    },
})
require("dap-python").setup("/home/oleete/.local/share/nvim/mason/packages/debugpy/venv/bin/python")
require('dap-go').setup()
require("mason-nvim-dap").setup()

dap.adapters.codelldb = {
    type = "server",
    port = "${port}",
    executable = {
        command = "/home/oleete/.local/share/nvim/mason/bin/codelldb",
        args = { "--port", "${port}" },
    }
}

dap.configurations.cpp = {
    {
        name = "Launch file",
        type = "codelldb",
        request = "launch",
        program = function()
            return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
        end,
        cwd = "${workspaceFolder}",
        stopOnEntry = true,
    },
}
dap.configurations.c = dap.configurations.cpp

local dapui = require("dapui")
local debug_win = nil
local debug_tab = nil
local debug_tabnr = nil

local function open_in_tab()
    if debug_win and vim.api.nvim_win_is_valid(debug_win) then
        vim.api.nvim_set_current_win(debug_win)
        return
    end

    vim.cmd("tabedit %")
    debug_win = vim.fn.win_getid()
    debug_tab = vim.api.nvim_win_get_tabpage(debug_win)
    debug_tabnr = vim.api.nvim_tabpage_get_number(debug_tab)

    vim.t[vim.api.nvim_get_current_tabpage()].tabname = "Debugging"

    dapui.open()
end

local function close_tab()
    dapui.close()

    if debug_tab and vim.api.nvim_tabpage_is_valid(debug_tab) then
        vim.api.nvim_exec("tabclose " .. debug_tabnr, false)
    end

    debug_win = nil
    debug_tab = nil
    debug_tabnr = nil
end

-- Attach DAP UI to DAP events
dap.listeners.after.event_initialized["dapui_config"] = function()
    open_in_tab()
end
dap.listeners.before.event_terminated["dapui_config"] = function()
    close_tab()
end
dap.listeners.before.event_exited["dapui_config"] = function()
    close_tab()
end
