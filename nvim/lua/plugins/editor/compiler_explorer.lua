---@module "lazy"
---@type LazySpec
return {
    "krady21/compiler-explorer.nvim",
    opts = {
        line_match = {
            highlight = true,
            jump = true,
        },
        split = "vsplit",
    },
    cmd = {
        "CECompile",
        "CECompileLive",
        "CEFormat",
        "CEAddLibrary",
        "CELoadExample",
        "CEOpenWebsite",
        "CEDeleteCache",
        "CEShowTooltip",
        "CEGotoLabel",
    },
    keys = {
        { "<leader>xx", "<cmd>CECompile<cr>",     desc = "Open compiler explorer" },
        { "<leader>xl", "<cmd>CECompileLive<cr>", desc = "Live compiler explorer" },
        { "<leader>xf", "<cmd>CEFormat<cr>",      desc = "Format" },
        { "<leader>xk", "<cmd>CEShowTooltip<cr>", desc = "Tooltip" },
        { "<leader>xg", "<cmd>CEGotoLabel<cr>",   desc = "Goto" },
        { "<leader>xw", "<cmd>CEOpenWebsite<cr>", desc = "Open website" },
    },
}
