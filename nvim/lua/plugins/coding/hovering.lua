---@module "lazy"
---@type LazySpec
return {
    {
        "lewis6991/hover.nvim",
        opts = {
            init = function()
                require("hover.providers.lsp")
                require("hover.providers.gh")
                require("hover.providers.gh_user")
                require("hover.providers.dap")
                require("hover.providers.diagnostic")
                require("hover.providers.man")
            end,
            preview_opts = {
                border = vim.o.winborder,
            },
            preview_window = false,
            title = true,
            mouse_providers = { "LSP" },
            mouse_delay = 1000,
        },
        keys = {
            { "K", function() require("hover").hover() end, desc = "hover.nvim" }, ---@diagnostic disable-line: missing-parameter
        },
    },
}
