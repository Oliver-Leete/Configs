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
        end
    },
    {
        "hrsh7th/nvim-cmp",
        optional = true,
        dependencies = { "saadparwaiz1/cmp_luasnip" },
        opts = function(_, opts)
            opts.snippet = {
                expand = function(args)
                    require("luasnip").lsp_expand(args.body)
                end,
            }
            table.insert(opts.sources, { name = "luasnip" })
        end,
        keys = {
            { "<tab>",   function() require("luasnip").jump(1) end,  mode = "s" },
            { "<s-tab>", function() require("luasnip").jump(-1) end, mode = { "i", "s" } },
        },
    },
}
