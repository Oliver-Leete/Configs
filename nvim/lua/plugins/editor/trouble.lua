---@module "lazy"
---@type LazySpec
return {
    "folke/trouble.nvim",
    opts = {
        warn_no_restuls = false,
        open_no_results = true,
        preview = {
            type = "main",
        },
    },
    lazy = false,
    cmd = "Trouble",
    keys = {
        { "<leader>l", function() require("user.myfuncs").trouble_snacks("toggle") end, desc = "Toggle trouble" },

        {
            "[l",
            function()
                require("user.targets").func(require("trouble").prev, "l", { jump = true, skip_groups = true, })
            end,
            desc = "Trouble item"
        },
        {
            "]l",
            function()
                require("user.targets").func(require("trouble").next, "l", { jump = true, skip_groups = true, })
            end,
            desc = "Trouble item"
        },
    },
    specs = {
        "folke/snacks.nvim",
        opts = function(_, opts)
            return vim.tbl_deep_extend("force", opts or {}, {
                picker = {
                    actions = vim.tbl_extend("force", require("trouble.sources.snacks").actions, {
                        trouble_close = { action = function() require("user.myfuncs").trouble_snacks("close") end },
                        trouble_f_open = { action = function() require("user.myfuncs").trouble_snacks("open") end }
                    }),
                    win = {
                        input = {
                            keys = {
                                ["<c-l>"] = { { "trouble_close", "trouble_open", "trouble_f_open" }, mode = { "n", "i" }, },
                                ["<m-l>"] = { { "trouble_close", "trouble_add", "trouble_f_open" }, mode = { "n", "i" }, },
                            },
                        },
                    },
                },
            })
        end,
    }
}
