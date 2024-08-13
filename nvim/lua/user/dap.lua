local dap = require("dap")
require("dap.ext.vscode").load_launchjs()

vim.fn.sign_define("DapBreakpoint", { text = "", texthl = "DiagnosticSignError", culhl = "CursorLineError" })
vim.fn.sign_define("DapBreakpointCondition", { text = "", texthl = "DiagnosticSignError", culhl = "CursorLineError" })
vim.fn.sign_define("DapLogPoint", { text = "", texthl = "DiagnosticSignWarn", culhl = "CursorLineWarn" })
vim.fn.sign_define("DapStopped", { text = "→", texthl = "DiagnosticSignError", culhl = "CursorLineError" })
vim.fn.sign_define("DapBreakpointRejected", { text = "", texthl = "DiagnosticSignInfo", culhl = "CursorLineInfo" })

require("nvim-dap-virtual-text").setup()
require("dapui").setup({
    floating = {
        border = require("user.settings").border,
    },
    controls = {
        enabled = false,
        icons = {
            pause = "",
            play = " ",
            step_into = "",
            step_over = " ",
            step_out = "",
            step_back = " ",
            run_last = " ",
            terminate = " ",
        },
    },
})
require("dap-python").setup("/home/oleete/.local/share/nvim/mason/packages/debugpy/venv/bin/python")
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

local open_in_tab = function()
    if debug_win and vim.api.nvim_win_is_valid(debug_win) then
        vim.api.nvim_set_current_win(debug_win)
        return
    end

    vim.cmd("tabedit %")
    debug_win = vim.fn.win_getid()
    debug_tab = vim.api.nvim_win_get_tabpage(debug_win)
    debug_tabnr = vim.api.nvim_tabpage_get_number(debug_tab)

    dapui.open()
end

local close_tab = function()
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
