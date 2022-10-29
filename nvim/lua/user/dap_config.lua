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

-- Mappings
--
-- Make it so that this changes all the , leader keys to debug keys when active but is exitable (maybe with ,<esc>)
--
-- Hydra({
--     name = "Debug",
--     mode = { "n" },
--     body = "<leader>d",
--     config = {
--         hint = {
--             position = "middle-right",
--             border = "single"
--         }
--     },
--     --     hint = [[
--     --  _h_/_j_/_k_/_l_: ←/↓/↑/→
--     --  _H_/_J_/_K_/_L_: ⇚/⟱/⤊/⇛
--     --        _t_: top
--     --        _v_: middle
--     --        _b_: bottom
--     --        _s_: start
--     --        _m_: middle
--     --        _e_: end
--     -- ]]   ,
--     heads = {
--
--         { ",d", dap.continue },
--         { ",n", dap.continue },
--         { ",D", dap.run_last, { exit = true } },
--         { ",h", dap.step_back },
--         { ",j", dap.step_into },
--         { ",k", dap.step_out },
--         { ",l", dap.step_over },
--         { ",p", dap.pause },
--         { ",<Up>", dap.up },
--         { ",<Down>", dap.down },
--         { ",e", dap.run_to_cursor },
--         { ",b", dap.toggle_breakpoint },
--         { ",", function() dap.set_breakpoint(vim.fn.input('Breakpoint condition: ')) end },
--         { ",", function() dap.set_breakpoint(nil, nil, vim.fn.input('Log point message: ')) end },
--     }
-- })
-- Map("n", "<leader>dd", dap.continue)
-- Map("n", "<leader>dD", dap.run_last)
