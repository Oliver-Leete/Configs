---@module "lazy"
---@type LazySpec
return {
    "Wansmer/sibling-swap.nvim",
    requires = { "nvim-treesitter/nvim-treesitter" },
    opts = {
        use_default_keymaps = false,
    },
    keys = {
        { "(", function() require("sibling-swap").swap_with_left() end, desc = "Swap with left" },
        { ")", function() require("sibling-swap").swap_with_right() end, desc = "Swap with right" },
    },
}
