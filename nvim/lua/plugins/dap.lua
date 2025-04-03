local dap_setup = function()
    local dap = require("dap")
    require("dap.ext.vscode").load_launchjs()

    vim.fn.sign_define("DapBreakpoint", { text = "", texthl = "DiagnosticSignError", culhl = "CursorLineError" })
    vim.fn.sign_define("DapBreakpointCondition",
        { text = "", texthl = "DiagnosticSignError", culhl = "CursorLineError" })
    vim.fn.sign_define("DapLogPoint", { text = "", texthl = "DiagnosticSignWarn", culhl = "CursorLineWarn" })
    vim.fn.sign_define("DapStopped", { text = "→", texthl = "DiagnosticSignError", culhl = "CursorLineError" })
    vim.fn.sign_define("DapBreakpointRejected", { text = "", texthl = "DiagnosticSignInfo", culhl = "CursorLineInfo" })

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
end

return {
    "mfussenegger/nvim-dap",
    config = dap_setup,
    dependencies = {
        { "theHamsta/nvim-dap-virtual-text", opts = {}, lazy = true },
        { "liadOz/nvim-dap-repl-highlights", opts = {}, lazy = true },
        {
            "igorlfs/nvim-dap-view",
            config = function()
                local dap, dv = require("dap"), require("dap-view")
                dap.listeners.before.attach["dap-view-config"] = dv.open
                dap.listeners.before.launch["dap-view-config"] = dv.open
                dap.listeners.before.event_terminated["dap-view-config"] = dv.close
                dap.listeners.before.event_exited["dap-view-config"] = dv.close
                local dap_view_sections = {
                    { name = "watches",     desc = "Watches",     keymap = "W", },
                    { name = "breakpoints", desc = "Breakpoints", keymap = "B", },
                    { name = "exceptions",  desc = "Exceptions",  keymap = "E", },
                    { name = "threads",     desc = "Threads",     keymap = "T", },
                    { name = "repl",        desc = "REPL",        keymap = "R", },
                }
                local dap_view_state = require("dap-view.state")
                local dap_view_renderer = function(props)
                    local gui = require("user.colors").colors(props)
                    local ret = {}
                    for i, section in ipairs(dap_view_sections) do
                        if dap_view_state.current_section == section.name then
                            ret[(i * 3) - 2] = { "", guifg = gui.bg, guibg = gui.fg }
                            ret[(i * 3) - 1] = {
                                section.desc .. " [" .. section.keymap .. "]",
                                guifg = gui.fg,
                                guibg =
                                    gui.bg
                            }
                            ret[(i * 3) - 0] = { "", guifg = gui.bg, guibg = gui.fg }
                        else
                            ret[(i * 3) - 2] = { " ", guifg = gui.bg, guibg = "#1F1F28" }
                            ret[(i * 3) - 1] = {
                                section.desc .. " [" .. section.keymap .. "]",
                                guifg = gui.bg,
                                guibg =
                                "#1F1F28"
                            }
                            ret[(i * 3) - 0] = { " ", guifg = gui.bg, guibg = "#1F1F28" }
                        end
                    end
                    return ret
                end
                local dv_group = vim.api.nvim_create_augroup("dap_view", {})
                vim.api.nvim_create_autocmd({ "Filetype" }, {
                    pattern = "dap-view",
                    group = dv_group,
                    callback = function()
                        vim.b[0].incline_renderer = dap_view_renderer
                    end
                })
            end
        },
        {
            "mfussenegger/nvim-dap-python",
            config = function()
                require("dap-python").setup("/home/oleete/.local/pipx/venvs/debugpy/bin/python")
            end,
            lazy = true,
        },
        { "kdheepak/nvim-dap-julia", opts = {}, },
    },
    keys = {

        -- Stepping
        { "<leader>dh", function() require("dap").step_out() end,          desc = "step out" },
        { "<leader>dj", function() require("dap").step_over() end,         desc = "step over" },
        { "<leader>dk", function() require("dap").step_back() end,         desc = "step back" },
        { "<leader>dl", function() require("dap").step_into() end,         desc = "step in" },
        { "<leader>d<", function() require("dap").up() end,                desc = "step up" },
        { "<leader>d>", function() require("dap").down() end,              desc = "step down" },

        -- Breakpoints
        { "<leader>db", function() require("dap").toggle_breakpoint() end, desc = "toggle breakpoint" },
        {
            "<leader>dB",
            function()
                require("dap").set_breakpoint(
                    vim.fn.input("Breakpoint condition: "),
                    vim.fn.input("Hit condition: "),
                    vim.fn.input("Log message: ")
                )
            end,
            desc = "set conditional breakpoint"
        },

        -- Starting/Stopping
        { "<leader>dd", function() require("dap").continue() end,      desc = "run" },
        { "<leader>dD", function() require("dap").run_to_cursor() end, desc = "run to cursor" },
        { "<leader>dn", function() require("dap").continue() end,      desc = "run" },
        { "<leader>dN", function() require("dap").continue() end,      desc = "run backwards" },
        { "<leader>dp", function() require("dap").pause() end,         desc = "pause" },
        {
            "<leader>ds",
            function()
                local widgets = require("dap.ui.widgets")
                widgets.centered_float(widgets.scopes, { border = "rounded" })
            end,
            desc = "view scopes"
        },
        { "<leader>dx", function() require("dap").terminate() end, desc = "terminate" },
    }
}
