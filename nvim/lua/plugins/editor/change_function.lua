---@module "lazy"
---@type LazySpec
return {
    'SleepySwords/change-function.nvim',
    dependencies = {
        'MunifTanjim/nui.nvim',
        'nvim-treesitter/nvim-treesitter',
        'nvim-treesitter/nvim-treesitter-textobjects',
    },
    keys = {
        { ",ra", function() require("change-function").change_function() end, desc = "Change function signature" }
    },
}
