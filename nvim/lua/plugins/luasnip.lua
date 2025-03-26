return {
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
    end

}
