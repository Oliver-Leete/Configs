---@module "lazy"
---@type LazySpec
return {
    "echasnovski/mini.icons",
    config = function()
        require("mini.icons").setup()
        MiniIcons.mock_nvim_web_devicons()
    end
}
