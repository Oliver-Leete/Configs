require("trouble").setup({
    position = "right",
    action_keys = {
        jump_close = "<cr>",
        close = { "<esc>", "q" },
        toggle_preview = "p"
    },
    signs = {
        other = " "
    },
    auto_preview = false,
    use_diagnostic_signs = true,
    fold_closed = "",
    fold_open = "",
})

require("edgy").setup({
    wo = {
        winhighlight = "",
        statuscolumn = "%s",
    },
    animate = {
        enabled = false,
        spinner = require("noice.util.spinners").spinners.circleFull,
    },
    options = {
        left = { size = 50 },
        bottom = { size = 30 },
    },
    bottom = {
        { ft = "dapui_watches", title = "Watches" },
        { ft = "dap-repl",      title = "Debug REPL" },
        { ft = "dapui_console", title = "Debug Console" },
        {
            ft = "Trouble",
            title = " Trouble",
            open = function() require("trouble").toggle({ mode = "quickfix" }) end,
        },
        {
            ft = "OverseerPanelTask",
            title = " Task",
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
