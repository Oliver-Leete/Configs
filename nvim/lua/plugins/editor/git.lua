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
            { "<leader>gdD", ":DiffviewOpen ", desc = "Diff current" },
        },
    },
}
