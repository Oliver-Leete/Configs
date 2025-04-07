return {
    "folke/trouble.nvim",
    opts = {
        warn_no_restuls = false,
        open_no_results = true,
        preview = {
            type = "main",
        },
    },
    cmd = "Trouble",
    keys = {
        { "<leader>m", function() require("trouble").toggle("snacks") end, desc = "Toggle trouble" },
    },
    specs = {
        "folke/snacks.nvim",
        opts = function(_, opts)
            return vim.tbl_deep_extend("force", opts or {}, {
                picker = {
                    actions = require("trouble.sources.snacks").actions,
                    win = {
                        input = {
                            keys = {
                                ["<c-L>"] = { "trouble_open", mode = { "n", "i" }, },
                            },
                        },
                    },
                },
            })
        end,
    }
}
