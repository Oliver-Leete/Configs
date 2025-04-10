---@module "lazy"
---@type LazySpec
return {
    "echasnovski/mini.misc",
    config = function()
        require("mini.misc").setup_restore_cursor({
            center = true
        })
    end
}
