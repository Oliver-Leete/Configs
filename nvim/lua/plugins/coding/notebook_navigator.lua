local ft = { "julia", "python" }
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
        repl_provider = "iron",
    },
    event = "VeryLazy",
    keys = {
        {
            "]m",
            function() require("user.targets").func(require("notebook-navigator").move_cell, "n", "d") end,
            desc = "Goto next cell",
            ft = ft,
        },
        {
            "[m",
            function() require("user.targets").func(require("notebook-navigator").move_cell, "n", "u") end,
            desc = "Notebook cell",
            ft = ft,
        },
        { "<leader>rc", function() require("notebook-navigator").run_cell() end, desc = "Run cell", ft = ft },
        { "<leader>rC", function() require("notebook-navigator").run_all_cells() end, desc = "Run all cells", ft = ft },
        {
            "<leader>rn",
            function() require("notebook-navigator").run_and_move() end,
            desc = "Run cell and advance",
            ft = ft,
        },

        { ",mj", function() require("notebook-navigator").swap_cell("d") end, desc = "Swap cell down", ft = ft },
        { ",mk", function() require("notebook-navigator").swap_cell("u") end, desc = "Swap cell up", ft = ft },
        { ",mJ", function() require("notebook-navigator").merge_cell("d") end, desc = "Merge cell down", ft = ft },
        { ",mK", function() require("notebook-navigator").merge_cell("u") end, desc = "Merge cell up", ft = ft },

        { ",mo", function() require("notebook-navigator").add_cell_below() end, desc = "Add cell below", ft = ft },
        { ",mO", function() require("notebook-navigator").add_cell_above() end, desc = "Add cell above", ft = ft },
    },
}
