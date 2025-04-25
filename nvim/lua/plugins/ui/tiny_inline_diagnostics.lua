---@module "lazy"
---@type LazySpec
return {
    "rachartier/tiny-inline-diagnostic.nvim",
    event = "VeryLazy",
    priority = 1000,
    opts = {
        preset = "powerline",
        transparent_bg = true,
        transparent_cursorline = true,
        options = {
            show_source = {
                enabled = true,
                if_many = true,
            },
            use_icons_from_diagnostic = true,
            add_messages = true,
            softwrap = 30,
            multilines = {
                enabled = true,
                always_show = true,
            },
            show_all_diags_on_cursorline = true,
        },
    },
}
