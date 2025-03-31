return {
    "folke/flash.nvim",
    opts = {
        labels = "tnseriaodhgjplfuwybkvmcxzq",
        label = { uppercase = false, rainbow = { enabled = true } },
        jump = { nohlsearch = true },
        modes = { search = { enabled = false }, char = { enabled = false } },
    },
    keys = {
        { "s", function() require("flash").jump() end,              mode = { "n", "x", "o" }, desc = "Flash" },
        { "z", function() require("flash").treesitter() end,        mode = { "n", "x", "o" }, desc = "Flash treesitter" },
        { "Z", function() require("flash").treesitter_search() end, mode = { "n", "x", "o" }, desc = "Treesitter search" },
        { "S", function() require("flash").remote() end,            mode = { "o" },           desc = "Remote flash" },
    }
}
