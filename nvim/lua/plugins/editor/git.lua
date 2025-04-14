---@module "lazy"
---@type LazySpec
return {
    {
        "sindrets/diffview.nvim",
        opts = {},
        cmd = {
            "DiffviewOpen",
            "DiffviewClose",
            "DiffviewFileHistory",
        }
    },
    {
        "NeogitOrg/neogit",
        dependencies = {
            "sindrets/diffview.nvim",
        },
        ---@module "neogit"
        ---@type NeogitConfig
        opts = {
            graph_style = "kitty",
            integrations = {
                diffview = true,
            },
        },
        keys = {
            { "<leader>gg", function() require("neogit").open() end, desc = "Open neogit" },
        },
    }
}
