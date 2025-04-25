---@module "lazy"
---@type LazySpec
return {
    {
        "folke/which-key.nvim",
        optional = true,
        opts = {
            to_add = {
                commenting = {
                    { ",c", icon = "󰆂 " },
                    { ",C", icon = "󰆃 " },
                    { ",o", icon = "󰧤 " },
                    { ",O", icon = "󰧢 " },
                    { ",b", icon = "󱀢 " },
                    { ",a", icon = "󰆆 " },
                },
            },
        },
    },
    {
        "numToStr/Comment.nvim",
        opts = { mappings = { basic = false, extra = false } },
        keys = {
            {
                "<c-/>",
                function()
                    return vim.v.count == 0 and "<Plug>(comment_toggle_linewise_current)"
                        or "<Plug>(comment_toggle_linewise_count)"
                end,
                expr = true,
                desc = "Toggle line comment",
            },
            { "<c-/>", "<plug>(comment_toggle_linewise_visual)", desc = "Toggle line comment", mode = "x" },

            { ",c", "<plug>(comment_toggle_linewise)", desc = "Toggle line comment" },
            { ",c", "<plug>(comment_toggle_linewise_visual)", desc = "Toggle line comment", mode = "x" },

            { ",b", "<plug>(comment_toggle_blockwise)", desc = "Toggle block comment" },
            { ",b", "<plug>(comment_toggle_blockwise_visual)", desc = "Toggle block comment", mode = "x" },

            { ",C", function() require("Comment.api").insert.eol() end, desc = "Comment at end of line" },
            { ",o", function() require("Comment.api").insert.linewise.below() end, desc = "Comment below" },
            { ",O", function() require("Comment.api").insert.linewise.above() end, desc = "Comment above" },
        },
    },
    {
        "folke/todo-comments.nvim",
        dependencies = { "nvim-lua/plenary.nvim" },
        event = { "VeryLazy" },
        opts = {
            signs = false,
            sign_priority = 2,
            keywords = {
                FIX = { icon = " ", color = "error", alt = { "FIXME", "BUG", "FIXIT", "FIX", "ISSUE" } },
                TODO = { icon = " ", color = "info" },
                HACK = { icon = " ", color = "warning", alt = { "JANK", "WORKAROUND" } },
                WARN = { icon = " ", color = "warning", alt = { "WARNING" } },
                PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
                NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
            },
            highlight = {
                before = "",
                keyword = "bg",
                after = "bg",
                pattern = [[.*<((KEYWORDS)%(\(.{-1,}\))?):?]],
            },
            search = {
                pattern = [[\b(KEYWORDS)(\(\w*\))*:?]],
            },
            colors = {
                error = { "LspDiagnosticsDefaultError", "ErrorMsg", "#DC2626" },
                warning = { "LspDiagnosticsDefaultWarning", "WarningMsg", "#FBBF24" },
                info = { "LspDiagnosticsDefaultInformation", "#2563EB" },
                hint = { "LspDiagnosticsDefaultHint", "#10B981" },
                default = { "Identifier", "#7C3AED" },
            },
        },
    },
}
