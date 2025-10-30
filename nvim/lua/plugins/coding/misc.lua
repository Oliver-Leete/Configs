---@module "lazy"
---@type LazySpec
return {
    {
        "okuuva/auto-save.nvim",
        opts = {
            condition = function(buf)
                return vim.bo[buf].modifiable
                    and not vim.list_contains({ "oil", "qf" }, vim.bo[buf].filetype)
            end,
        },
    },
    {
        "nvim-mini/mini.misc",
        config = function()
            require("mini.misc").setup_restore_cursor({
                center = true,
            })
        end,
    },
    {
        "nvim-mini/mini.hipatterns",
        opts = function()
            return {
                highlighters = {
                    hex_color = require("mini.hipatterns").gen_highlighter.hex_color(),
                },
            }
        end,
    },
}
