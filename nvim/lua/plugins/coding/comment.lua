---@module "lazy"
---@type LazySpec
return {
    "numToStr/Comment.nvim",
    opts = { mappings = { basic = false, extra = false } },
    keys = {
        {
            "<c-/>",
            function()
                return vim.v.count == 0 and "<Plug>(comment_toggle_linewise_current)"
                    or "<Plug>(comment_toggle_linewise_count)"
            end,
            expr = true,
            desc = "Toggle line comment",
        },
        { "<c-/>", "<plug>(comment_toggle_linewise_visual)", desc = "Toggle line comment", mode = "x" },

        { ",c", "<plug>(comment_toggle_linewise)", desc = "Toggle line comment" },
        { ",c", "<plug>(comment_toggle_linewise_visual)", desc = "Toggle line comment", mode = "x" },

        { ",b", "<plug>(comment_toggle_blockwise)", desc = "Toggle block comment" },
        { ",b", "<plug>(comment_toggle_blockwise_visual)", desc = "Toggle block comment", mode = "x" },

        { ",C", function() require("Comment.api").insert.eol() end, desc = "Comment at end of line" },
        { ",o", function() require("Comment.api").insert.linewise.below() end, desc = "Comment below" },
        { ",O", function() require("Comment.api").insert.linewise.above() end, desc = "Comment above" },
    },
}
