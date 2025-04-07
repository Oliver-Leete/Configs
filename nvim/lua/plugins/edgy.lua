return {
    {
        "lucobellic/edgy-group.nvim",
        event = "VeryLazy",
        dependencies = { "folke/edgy.nvim" },
        keys = {
            {
                "<leader>sl",
                function() require("edgy-group").open_group_offset("bottom", 1) end,
                desc = "Edgy Group Next bottom",
            },
            {
                "<leader>sh",
                function() require("edgy-group").open_group_offset("bottom", -1) end,
                desc = "Edgy Group Prev bottom",
            }, {
            "<leader>ss", "<cmd>EdgyGroupSelect<cr>", desc = "Edgy Group Pick",
        },
        },
        opts = {
            groups = {
                bottom = {
                    { icon = " ", titles = { "dap-view", "dap-repl", "dap-consol" }, pick_key = "d" },
                    { icon = " ", titles = { "terminal" }, pick_key = "t" },
                    { icon = " ", titles = { "overseer-list" }, pick_key = "r" },
                    { icon = "󰙨 ", titles = { "neotest-panel" }, pick_key = "t" },
                    { icon = " ", titles = { "trouble-diagnostics", "trouble-todo" }, pick_key = "x" },
                    { icon = " ", titles = { "trouble-snacks", "trouble-snacks-files" }, pick_key = "s" },
                },
                left = {
                    { icon = "󰙨 ", titles = { "neotest-list" }, pick_key = "T" },
                    { icon = " ", titles = { "trouble-lsp" }, pick_key = "l" },
                },
            },
            toggle = false,
        },
    },

    {
        "folke/edgy.nvim",
        opts = {
            options = {
                bottom = {
                    size = 20,
                },
            },
            close_when_all_hidden = false,
            exit_when_last = false,
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
                        return win_trouble and win_trouble.mode == "snacks"
                    end,
                    open = "Trouble snacks open",
                },
                {
                    title = "trouble-snacks-files",
                    ft = "trouble",
                    filter = function(_, win)
                        local win_trouble = vim.w[win].trouble
                        return win_trouble and win_trouble.mode == "snacks_files"
                    end,
                    open = "Trouble snacks_files open",
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
                    title = "terminal",
                    ft = "toggleterm",
                    open = function()
                        local last_term = require("toggleterm.terminal").last_opened
                        if last_term ~= nil then
                            last_term:open()
                        else
                            require("toggleterm").new_command()
                        end
                    end,
                },
                {
                    title = 'neotest-panel',
                    ft = 'neotest-output-panel',
                    open = 'Neotest output-panel',
                },

            },
            left = {
                {
                    title = 'neotest-list',
                    ft = 'neotest-summary',
                    open = 'Neotest summary',
                    size = { width = 0.20 },
                },
                {
                    title = "overseer-list",
                    ft = "OverseerList",
                    open = function()
                        require("overseer").open()
                    end,
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
    } }
