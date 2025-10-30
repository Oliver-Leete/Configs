---@module "lazy"
---@type LazySpec
return {
    "nvim-mini/mini.diff",
    lazy = false,
    opts = {
        view = { style = "number" },
        mappings = {
            apply = "<leader>gs",
            reset = "<leader>gr",
            textobject = "ih",
            goto_first = "",
            goto_prev = "",
            goto_next = "",
            goto_last = "",
        },
    },
    keys = {
        { "<leader>gS", function() require("mini.diff").do_hunks(0, "apply") end, desc = "Apply all hunks" },
        { "<leader>gR", function() require("mini.diff").do_hunks(0, "reset") end, desc = "Reset all hunks" },

        {
            "[h",
            function()
                if vim.wo.diff then
                    require("user.targets").mapping("[c", "h")
                else
                    require("user.targets").func(require("mini.diff").goto_hunk, "h", "prev")
                end
            end,
            mode = { "n", "x", "o" },
            desc = "Hunk",
        },
        {
            "]h",
            function()
                if vim.wo.diff then
                    require("user.targets").mapping("]c", "h")
                else
                    require("user.targets").func(require("mini.diff").goto_hunk, "h", "next")
                end
            end,
            mode = { "n", "x", "o" },
            desc = "Hunk",
        },
    },
}
