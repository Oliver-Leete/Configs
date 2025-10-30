---@module "lazy"
---@type LazySpec
return {
    "nvim-mini/mini.icons",
    config = function()
        require("mini.icons").setup()
        MiniIcons.mock_nvim_web_devicons()
    end,
}
