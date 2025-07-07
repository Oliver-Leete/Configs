---@module "lazy"
---@type LazySpec
return {
    {
        "folke/edgy.nvim",
        opts = {
            options = {
                bottom = {
                    size = 20,
                },
            },
            close_when_all_hidden = true,
            exit_when_last = true,
            animate = {
                enabled = false,
            },
            wo = {
                winbar = false,
                spell = false,
            },
            keys = {
                ["<c-q>"] = false,
                ["q"] = function(win) win:close() end,
                ["Q"] = function(win) win.view.edgebar:close() end,
            },
            bottom = {
                {
                    title = "dap-view",
                    ft = "dap-view",
                    open = function() require("dap-view").open() end,
                },
                {
                    title = "dap-repl",
                    ft = "dap-repl",
                    open = function() require("dap-view").open() end,
                },
                {
                    title = "dap-consol",
                    ft = "dap-view-term",
                    open = function() require("dap").repl.open() end,
                    collapsed = false,
                },
                {
                    title = "trouble-snacks",
                    ft = "trouble",
                    filter = function(_, win)
                        local win_trouble = vim.w[win].trouble
                        return win_trouble and (win_trouble.mode == "snacks" or win_trouble.mode == "snacks_files")
                    end,
                    open = function() require("user.myfuncs").trouble_snacks("open") end,
                },
                {
                    title = "trouble-diagnostics",
                    ft = "trouble",
                    filter = function(_, win)
                        local win_trouble = vim.w[win].trouble
                        return win_trouble and win_trouble.mode == "diagnostics"
                    end,
                    open = "Trouble diagnostics open",
                },
                {
                    title = "trouble-todo",
                    ft = "trouble",
                    filter = function(_, win)
                        local win_trouble = vim.w[win].trouble
                        return win_trouble and win_trouble.mode == "todo"
                    end,
                    open = "Trouble todo open",
                },
                {
                    title = "neotest-panel",
                    ft = "neotest-output-panel",
                    open = "Neotest output-panel",
                },
                {
                    title = "terminal",
                    ft = "toggleterm",
                    open = "ToggleTermLast",
                    filter = function(buf, _)
                        local term = require("toggleterm.terminal").find(function(t) return t.bufnr == buf end)
                        if term then return term.direction == "horizontal" end
                    end,
                },
            },
            left = {
                {
                    title = "overseer-list",
                    ft = "OverseerList",
                    size = { width = 0.15 },
                    open = function() require("overseer").open() end,
                },
                {
                    title = "neotest-list",
                    ft = "neotest-summary",
                    open = "Neotest summary",
                    size = { width = 0.20 },
                },
                {
                    title = "trouble-lsp",
                    ft = "trouble",
                    filter = function(_, win)
                        local win_trouble = vim.w[win].trouble
                        return win_trouble and win_trouble.mode == "lsp"
                    end,
                    open = "Trouble lsp open",
                },
            },
        },
    },
}
