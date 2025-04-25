---@module "lazy"
---@type LazySpec
return {
    "GCBallesteros/NotebookNavigator.nvim",
    dependencies = {
        "echasnovski/mini.comment",
        "akinsho/toggleterm.nvim",
        "anuvyklack/hydra.nvim",
    },
    opts = {
        repl_provider = "toggleterm",
    },
    event = "VeryLazy",
    keys = {
        {
            "]m",
            function() require("user.targets").func(require("notebook-navigator").move_cell, "n", "d") end,
            desc = "Goto next cell",
            ft = { "julia", "python" },
        },
        {
            "[m",
            function() require("user.targets").func(require("notebook-navigator").move_cell, "n", "u") end,
            desc = "Notebook cell",
            ft = { "julia", "python" },
        },
        { "<leader>rr", function() require("notebook-navigator").run_cell() end, desc = "Run cell" },
        { "<leader>rR", function() require("notebook-navigator").run_all_cells() end, desc = "Run all cells" },
        { "<leader>rn", function() require("notebook-navigator").run_and_move() end, desc = "Run cell and advance" },

        { ",mj", function() require("notebook-navigator").swap_cell("d") end, desc = "Swap cell down" },
        { ",mk", function() require("notebook-navigator").swap_cell("u") end, desc = "Swap cell up" },
        { ",mJ", function() require("notebook-navigator").merge_cell("d") end, desc = "Merge cell down" },
        { ",mK", function() require("notebook-navigator").merge_cell("u") end, desc = "Merge cell up" },

        { ",mo", function() require("notebook-navigator").add_cell_below() end, desc = "Add cell below" },
        { ",mO", function() require("notebook-navigator").add_cell_above() end, desc = "Add cell above" },
    },
}
