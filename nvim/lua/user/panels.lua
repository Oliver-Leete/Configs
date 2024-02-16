require("edgy").setup({
    wo = {
        winhighlight = "",
        statuscolumn = "%s",
    },
    animate = {
        enabled = false,
    },
    options = {
        left = {
            size = function()
                return math.max(math.min(math.ceil(vim.api.nvim_list_uis()[1].width / 5), 50), 30)
            end
        },
        bottom = {
            size = function()
                return math.max(math.min(math.ceil(vim.api.nvim_list_uis()[1].height / 5), 40), 20)
            end
        }

    },
    bottom = {
        { ft = "dapui_watches", title = "Watches" },
        { ft = "dap-repl", title = "Debug REPL" },
        { ft = "dapui_console", title = "Debug Console" },
        { ft = "qf", title = " QuickFix" },
        {
            ft = "OverseerPanelTask",
            title = "%{%v:lua.OverseerTask()%}",
            open = "OverseerQuickAction open",
        },
        {
            ft = "NoiceHistory",
            title = " Log",
            open = function() require("user.myfuncs").toggle_noice() end,
        },
        {
            ft = "neotest-output-panel",
            title = " Test Output",
            open = function()
                vim.cmd.vsplit(); require("neotest").output_panel.toggle()
            end,
        },
        {
            ft = "DiffviewFileHistory",
            title = " Diffs",
        },
    },
    left = {
        {
            ft = "help",
            size = { width = 80 },
            -- only show help buffers
            filter = function(buf)
                return vim.bo[buf].buftype == "help"
            end,
        },
        { ft = "dapui_scopes",      title = "Scopes" },
        { ft = "dapui_breakpoints", title = "Breakpoints" },
        { ft = "dapui_stacks",      title = "Stacks" },
        {
            ft = "DiffviewFiles",
            title = " Diffs",
        },
        {
            ft = "OverseerList",
            title = "  Tasks",
            open = "OverseerOpen",
        },
        {
            ft = "neotest-summary",
            title = "  Tests",
            open = function() require("neotest").summary.toggle() end,
        },
    },
    keys = {
        ["<esc>"] = function(win)
            win.view.edgebar:close()
        end,
    },
})
