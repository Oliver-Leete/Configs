return {
    "folke/flash.nvim",
    opts = {
        labels = "tnseriaodhgjplfuwybkvmcxzq",
        label = { uppercase = false, rainbow = { enabled = true } },
        jump = { nohlsearch = true },
        modes = { search = { enabled = false }, char = { enabled = false } },
    },
    keys = {
        { "s", function() require("flash").jump() end,       mode = { "n", "x", "o" }, desc = "Jump to a given string" },
        { "z", function() require("flash").treesitter() end, mode = { "n", "o" }, desc = "Select a treesitter object" },
        { "S", function() require("flash").remote() end,     mode = { "o" }, desc = "Remote" },
    }
}
