---@module "lazy"
---@type LazySpec
return {
    {
        "L3MON4D3/LuaSnip",
        dependencies = {
            { "iurimateus/luasnip-latex-snippets.nvim" },
        },
        config = function()
            require("luasnip").config.set_config({
                history = false,
                update_events = "TextChanged,TextChangedI",
                delete_check_events = "TextChanged",
            })
            require("luasnip.loaders.from_lua").load({ paths = "/home/oleete/.config/nvim/snippets" })
            require("luasnip-latex-snippets").setup()
        end,
        keys = {
            {
                "<c-n>",
                function()
                    local ls = require("luasnip")
                    if ls.choice_active() then
                        ls.change_choice(1)
                    end
                end,
                mode = { "i", "s" }
            }
        },
    },
    {
        "saghen/blink.cmp",
        optional = true,
        opts = {
            snippets = {
                preset = "luasnip",
            },
        },
    },
}
