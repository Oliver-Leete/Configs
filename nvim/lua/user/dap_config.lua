local dap = require("dap")

vim.fn.sign_define("DapBreakpoint", { text = "", texthl = "DiagnosticSignError", culhl = "CursorLineError" })
vim.fn.sign_define("DapBreakpointCondition", { text = "", texthl = "DiagnosticSignError", culhl = "CursorLineError" })
vim.fn.sign_define("DapLogPoint", { text = "", texthl = "DiagnosticSignWarn", culhl = "CursorLineWarn" })
vim.fn.sign_define("DapStopped", { text = "→", texthl = "DiagnosticSignError", culhl = "CursorLineError" })
vim.fn.sign_define("DapBreakpointRejected", { text = "", texthl = "DiagnosticSignInfo", culhl = "CursorLineInfo" })

require("nvim-dap-virtual-text").setup()
require("dapui").setup({
    floating = {
        border = Border,
    },
    controls = {
        enabled = false,
        icons = {
            pause = " ",
            play = " ",
            step_into = " ",
            step_over = " ",
            step_out = " ",
            step_back = " ",
            run_last = " ",
            terminate = " ",
        },
    },
    layouts = {
        {
            elements = {
                "breakpoints",
                "stacks",
                "scopes",
                "watches",
            },
            size = 0.2,
            position = "left",
        },
        {
            elements = {
                "repl",
                "console",
            },
            size = 0.3,
            position = "bottom",
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

dap.adapters.juliadb = {
    type = "executable",
    command = "/usr/bin/julia",
    args = {
        "--color=yes",
        "--startup-file=no",
        "--history-file=no",
        "--project",
        "/home/oleete/Projects/julia-vscode/scripts/debugger/run_debugger.jl",
    },
}

dap.configurations.julia = {
    {
        name = "Run active Julia file (stop on enter)",
        type = "juliadb",
        request = "attach",
        program = "${file}",
        cwd = "${workspaceFolder}",
        stopOnEntry = true,
    },
    {
        name = "Run active Julia file",
        type = "juliadb",
        request = "attach",
        program = "${file}",
        cwd = "${workspaceFolder}",
        stopOnEntry = false,
    },
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

dap.adapters.bashdb = {
    type = 'executable';
    command = vim.fn.stdpath("data") .. '/mason/packages/bash-debug-adapter/bash-debug-adapter';
    name = 'bashdb';
}

dap.configurations.sh = {
    {
        type = 'bashdb';
        request = 'launch';
        name = "Launch file";
        showDebugOutput = true;
        pathBashdb = vim.fn.stdpath("data") .. '/mason/packages/bash-debug-adapter/extension/bashdb_dir/bashdb';
        pathBashdbLib = vim.fn.stdpath("data") .. '/mason/packages/bash-debug-adapter/extension/bashdb_dir';
        trace = true;
        file = "${file}";
        program = "${file}";
        cwd = '${workspaceFolder}';
        pathCat = "cat";
        pathBash = "/bin/bash";
        pathMkfifo = "mkfifo";
        pathPkill = "pkill";
        args = {};
        env = {};
        terminalKind = "integrated";
    }
}

local hint = [[
  ^^^^        Step
^^^^━━━━━━━━━━━━━━━━━━━━━━━
 ^^^^        back
 ^^          _K_
     out _H_ ^^  _L_ into
 ^^          _J_
 ^^         over
   ns-out _<_  _>_ ns-in

 ^^        Action
^^^^━━━━━━━━━━━━━━━━━━━━━━━
 ^^_t_: toggle breakpoint
 ^^_T_: special breakpoint
 ^^_r_: continue
 ^^_R_: continue to cursor
 ^^_p_: pause
 ^^_X_: terminate

 _<esc>_: exit
]]
DapHydra = require('hydra')({
    name = 'Debug',
    hint = hint,
    config = {
        color = 'pink',
        invoke_on_body = true,
        hint = {
            border = Border,
            position = "middle-right"
        },
    },
    mode = { 'n' },
    body = '<leader>d',
    heads = {
        { 'H', dap.step_out, { desc = 'step out' } },
        { 'J', dap.step_over, { desc = 'step over' } },
        { 'K', dap.step_back, { desc = 'step back' } },
        { 'L', dap.step_into, { desc = 'step into' } },
        { 't', dap.toggle_breakpoint, { desc = 'toggle breakpoint' } },
        { "T", function()
            local cond = vim.fn.input('Breakpoint condition: ')
            local hit = vim.fn.input('Hit condition: ')
            local log = vim.fn.input('Log message: ')
            dap.set_breakpoint(cond, hit, log)
        end },
        { "<", dap.up },
        { ">", dap.down },
        { 'r', dap.continue, { desc = 'continue' } },
        { "R", dap.run_to_cursor },
        { 'X', dap.terminate, { desc = 'terminate' } },
        { "p", dap.pause },
        { '<esc>', nil, { exit = true, nowait = true, desc = 'exit' } },
        { '<leader>d', nil, { exit = true, nowait = true, desc = false } },
    }
})

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
    DapHydra:exit()

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

