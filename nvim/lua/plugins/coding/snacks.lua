---@module "lazy"
---@type LazySpec
return {
    {
        "folke/snacks.nvim",
        lazy = false,
        ---@module "snacks"
        ---@type snacks.Config
        opts = {
            toggle = {},
            zen = {
                toggles = {
                    dim = true,
                    mini_diff_signs = true,
                    diagnostics = true,
                    inlay_hints = true,
                },

                win = {
                    zindex = 51,
                },
                zoom = {
                    win = {
                        zindex = 51,
                    },
                },
            },
            scroll = { enabled = true },
            notifier = { enabled = true },
            input = { enabled = true },
            image = {
                enabled = true,
                doc = {
                    inline = true,
                    max_width = 120,
                    max_height = 100,
                },
            },
            lazygit = {
                win = { position = "float" },
                config = { os = { editPreset = "nvim-remote" }, gui = { nerdFontsVersion = "3" } },
            },
            bigfile = { enabled = true },
            words = { enabled = true },
        },
        keys = {
            {
                "<leader>gB",
                function() require("snacks.gitbrowse")() end,
                desc = "Git Browse",
                mode = { "n", "v" },
            },
            { "<leader>sn", function() require("snacks.notifier").show_history() end, desc = "Notification History" },
            {
                "]}",
                function() require("user.targets").func(require("snacks.words").jump, "}", vim.v.count1) end,
                desc = "Reference",
                mode = { "n", "t" },
            },
            {
                "[{",
                function() require("user.targets").func(require("snacks.words").jump, "{", -vim.v.count1) end,
                desc = "Reference",
                mode = { "n", "t" },
            },
        },
        init = function()
            vim.api.nvim_create_autocmd("User", {
                pattern = "VeryLazy",
                callback = function()
                    _G.dd = function(...) require("snacks.debug").inspect(...) end
                    _G.bt = function() require("snacks.debug").backtrace() end
                    vim.print = _G.dd

                    require("snacks.toggle").inlay_hints():map("<leader>zh")
                    require("snacks.toggle").indent():map("<leader>zi")
                    require("snacks.toggle").dim():map("<leader>zd")

                    require("snacks.toggle").words():map("<leader>zw")
                    require("snacks.toggle").option("wrap", { name = "Wrap" }):map("<leader>zW")
                    require("snacks.toggle").line_number():map("<leader>zl")
                    require("snacks.toggle")
                        .option("conceallevel", { off = 0, on = vim.o.conceallevel > 0 and vim.o.conceallevel or 2 })
                        :map("<leader>zH")

                    require("snacks.toggle").zen():map("<leader>zx")
                    require("snacks.toggle").zoom():map("<leader>zz")

                    require("snacks.toggle").diagnostics():map("<leader>zD")
                    require("snacks.toggle").treesitter():map("<leader>zT")
                    require("snacks.toggle")
                        .option("background", { off = "light", on = "dark", name = "Dark Background" })
                        :map("<leader>zB")
                end,
            })
        end,
    },
}
