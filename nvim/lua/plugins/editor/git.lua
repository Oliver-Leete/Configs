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
        },
        keys = {
            { "<leader>gdd", "<cmd>DiffviewFileHistory<cr>", desc = "File Histor" },
            { "<leader>gdD", ":DiffviewOpen ", desc = "File Histor" },
        },
    },
    {
        "NeogitOrg/neogit",
        dependencies = {
            "sindrets/diffview.nvim",
        },
        cmd = {
            "Neogit",
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
    },
}
