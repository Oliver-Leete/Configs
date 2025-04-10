local widget = function(widget_type)
    local widgets = require("dap.ui.widgets")
    widgets.centered_float(widgets[widget_type], { border = vim.o.winborder })
end

local set_bp = function()
    require("dap").set_breakpoint(
        vim.fn.input("Breakpoint condition: "),
        vim.fn.input("Hit condition: "),
        vim.fn.input("Log message: ")
    )
end

---@module "lazy"
---@type LazySpec
return {
    {
        "mfussenegger/nvim-dap",
        dependencies = {
            { "jay-babu/mason-nvim-dap.nvim", },
            { "theHamsta/nvim-dap-virtual-text", opts = {} },
            { "liadOz/nvim-dap-repl-highlights", opts = {} },
        },
        init = function()
            local signs = {
                DapBreakpoint = { text = "", texthl = "DiagnosticSignError", culhl = "CursorLineError" },
                DapBreakpointCondition = { text = "", texthl = "DiagnosticSignError", culhl = "CursorLineError" },
                DapLogPoint = { text = "", texthl = "DiagnosticSignWarn", culhl = "CursorLineWarn" },
                DapStopped = { text = "→", texthl = "DiagnosticSignError", culhl = "CursorLineError" },
                DapBreakpointRejected = { text = "", texthl = "DiagnosticSignInfo", culhl = "CursorLineInfo" },
            }
            for sign, opts in pairs(signs) do
                vim.fn.sign_define(sign, opts)
            end

            vim.api.nvim_create_autocmd("FileType", {
                group = vim.api.nvim_create_augroup("user_dap_autocmds", {}),
                pattern = "dap-float",
                callback = function(en)
                    vim.keymap.set("n", "<esc>", "<cmd>q<cr>", { desc = "Close window", buffer = en.buf })
                end
            })
        end,
        keys = {
            -- Stepping
            { "<leader>dh", function() require("dap").step_out() end,                          desc = "Step out" },
            { "<leader>dj", function() require("dap").step_over() end,                         desc = "Step over" },
            { "<leader>dk", function() require("dap").step_back() end,                         desc = "Step back" },
            { "<leader>dl", function() require("dap").step_into() end,                         desc = "Step in" },
            { "<leader>dL", function() require("dap").step_into({ askForTargets = true }) end, desc = "Step in" },
            { "<leader>d<", function() require("dap").up() end,                                desc = "Step up" },
            { "<leader>d>", function() require("dap").down() end,                              desc = "Step down" },
            { "<leader>dg", function() require("dap").goto_() end,                             desc = "Go to line (no execute)" },

            { "<leader>df", function() require("dap").focus_frame() end,                       desc = "Focus frame" },

            -- Breakpoints
            { "<leader>db", function() require("dap").toggle_breakpoint() end,                 desc = "Toggle breakpoint" },
            { "<leader>dB", set_bp,                                                            desc = "Set conditional breakpoint" },

            -- Viewing
            { "<leader>ds", function() widget("scopes") end,                                   desc = "View scopes" },
            { "<leader>dS", function() widget("sessions") end,                                 desc = "View sessions" },

            -- Running
            { "<leader>dn", function() require("dap").continue() end,                          desc = "Run" },
            { "<leader>dN", function() require("dap").run_to_cursor() end,                     desc = "Run to cursor" },
            { "<leader>de", function() require("dap").reverse_continue() end,                  desc = "Run backwards" },

            -- Starting/Stopping
            { "<leader>dd", function() require("dap").continue({ new = true }) end,            desc = "Start new" },
            { "<leader>dD", function() require("dap").run_last() end,                          desc = "Start last" },

            { "<leader>dp", function() require("dap").pause() end,                             desc = "Pause" },

            { "<leader>dr", function() require("dap").restart_frame() end,                     desc = "Restart frame" },
            { "<leader>dR", function() require("dap").restart() end,                           desc = "Restart" },

            { "<leader>dx", function() require("dap").terminate() end,                         desc = "End session" },
        }
    },
    {
        "jay-babu/mason-nvim-dap.nvim",
        dependencies = "mason.nvim",
        cmd = { "DapInstall", "DapUninstall" },
        opts = {
            automatic_installation = false,
            handlers = {},
            ensure_installed = {},
        },
    },
}
