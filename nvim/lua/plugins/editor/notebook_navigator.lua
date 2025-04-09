return {
    "GCBallesteros/NotebookNavigator.nvim",
    keys = {
        { "]m",         function() require("notebook-navigator").move_cell("d") end,   desc = "Goto next cell" },
        { "[m",         function() require("notebook-navigator").move_cell("u") end,   desc = "Goto prev cell" },
        { "<leader>rr", function() require('notebook-navigator').run_cell() end,       desc = "Run cell" },
        { "<leader>rR", function() require('notebook-navigator').run_all_cells() end,  desc = "Run all cells" },
        { "<leader>rn", function() require('notebook-navigator').run_and_move() end,   desc = "Run cell and advance" },

        { "<leader>rj", function() require('notebook-navigator').swap_cell("d") end,   desc = "Swap cell down" },
        { "<leader>rk", function() require('notebook-navigator').swap_cell("u") end,   desc = "Swap cell up" },
        { "<leader>rJ", function() require('notebook-navigator').merge_cell("d") end,  desc = "Merge cell down" },
        { "<leader>rK", function() require('notebook-navigator').merge_cell("u") end,  desc = "Merge cell up" },

        { "<leader>ro", function() require('notebook-navigator').add_cell_below() end, desc = "Add cell below" },
        { "<leader>rO", function() require('notebook-navigator').add_cell_above() end, desc = "Add cell above" },

        { "<leader>rc", function() require('notebook-navigator').comment_cell() end,   desc = "Comment out this cell" },
    },
    dependencies = {
        "echasnovski/mini.comment",
        "akinsho/toggleterm.nvim",
        "anuvyklack/hydra.nvim",
    },
    event = "VeryLazy",
    opts = {
        repl_provider = "toggleterm",
    },
}
