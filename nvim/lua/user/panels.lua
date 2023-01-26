require("todo-comments").setup({
    signs = true,
    sign_priority = 20,
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
    },
    colors = {
        error = { "LspDiagnosticsDefaultError", "ErrorMsg", "#DC2626" },
        warning = { "LspDiagnosticsDefaultWarning", "WarningMsg", "#FBBF24" },
        info = { "LspDiagnosticsDefaultInformation", "#2563EB" },
        hint = { "LspDiagnosticsDefaultHint", "#10B981" },
        default = { "Identifier", "#7C3AED" },
    },
})
