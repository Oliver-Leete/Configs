return {
    {
        "mfussenegger/nvim-dap",
        init = function()
            vim.fn.sign_define("DapBreakpoint", { text = "", texthl = "DiagnosticSignError", culhl = "CursorLineError" })
            vim.fn.sign_define("DapBreakpointCondition",
                { text = "", texthl = "DiagnosticSignError", culhl = "CursorLineError" })
            vim.fn.sign_define("DapLogPoint", { text = "", texthl = "DiagnosticSignWarn", culhl = "CursorLineWarn" })
            vim.fn.sign_define("DapStopped", { text = "→", texthl = "DiagnosticSignError", culhl = "CursorLineError" })
            vim.fn.sign_define("DapBreakpointRejected",
                { text = "", texthl = "DiagnosticSignInfo", culhl = "CursorLineInfo" })
        end,
        dependencies = {
            { "jay-babu/mason-nvim-dap.nvim", },
            { "theHamsta/nvim-dap-virtual-text", opts = {}, lazy = true },
            { "liadOz/nvim-dap-repl-highlights", opts = {}, lazy = true },
            { "kdheepak/nvim-dap-julia",         opts = {}, },
        },
        keys = {
            -- Stepping
            { "<leader>dh", function() require("dap").step_out() end,          desc = "step out" },
            { "<leader>dj", function() require("dap").step_over() end,         desc = "step over" },
            { "<leader>dk", function() require("dap").step_back() end,         desc = "step back" },
            { "<leader>dl", function() require("dap").step_into() end,         desc = "step in" },
            { "<leader>d<", function() require("dap").up() end,                desc = "step up" },
            { "<leader>d>", function() require("dap").down() end,              desc = "step down" },
            { "<leader>dg", function() require("dap").goto_() end,             desc = "Go to Line (No Execute)" },

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
                    widgets.centered_float(widgets.scopes, { border = vim.o.winborder })
                end,
                desc = "view scopes"
            },
            { "<leader>dx", function() require("dap").terminate() end, desc = "terminate" },
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
