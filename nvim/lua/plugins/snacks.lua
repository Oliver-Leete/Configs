return {
    "folke/snacks.nvim",
    opts = {
        toggle = {

        },
        picker = {
            layout = { preset = "ivy" },
            matcher = {
                frecency = true,
            },
            win = {
                input = {
                    keys = {
                        ["<c-x>"] = { "edit_split", mode = { "n", "i" }, },
                        ["<c-u>"] = "",
                        ["<c-a>"] = "",
                        ["<c-space>"] = { "toggle_live", mode = { "n", "i" } }
                    }
                }
            },
        },
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
        terminal = {
            shell = "fish",
            win = {
                position = "bottom",
            },
        },
        scroll = { enabled = true },
        notifier = { enabled = true },
        input = { enabled = true },
        image = { enabled = true },
        lazygit = { win = { position = "float" }, },
        bigfile = { enabled = true },
    },
    keys = {
        { "<leader>f",  function() require("snacks.picker").files({ hidden = true }) end, desc = "Find files" },
        { "<leader>F",  function() require("snacks.picker").resume() end,                 desc = "Resume last picker" },
        { "<leader>w",  function() require("snacks.picker").lsp_workspace_symbols() end,  desc = "Workspace symbols" },
        { "<leader>W",  function() require("snacks.picker").grep() end,                   desc = "Grep" },


        { "<leader>gg", function() require("snacks.lazygit")() end,                       desc = "Lazygit" },
        { "<leader>gl", function() require("snacks.lazygit").log() end,                   desc = "Log" },
        { "<leader>gL", function() require("snacks.lazygit").log_file() end,              desc = "File log" },
        { "<leader>gB", function() require("snacks.gitbrowse")() end,                     desc = "Git Browse",          mode = { "n", "v" } },

        { "<leader>R",  function() require("snacks.rename").rename_file() end,            desc = "Rename File" },

        { "<leader>st", function() require("snacks.terminal")() end,                      desc = "Toggle Terminal" },
        { "<leader>sn", function() require("snacks.notifier").show_history() end,         desc = "Notification History" },

        { "]]",         function() require("snacks.words").jump(vim.v.count1) end,        desc = "Next Reference",      mode = { "n", "t" } },
        { "[[",         function() require("snacks.words").jump(-vim.v.count1) end,       desc = "Prev Reference",      mode = { "n", "t" } },
    },
    init = function()
        vim.api.nvim_create_autocmd("User", {
            pattern = "VeryLazy",
            callback = function()
                _G.dd = function(...) require("snacks.debug").inspect(...) end
                _G.bt = function() require("snacks.debug").backtrace() end
                vim.print = _G.dd

                require("snacks.toggle").inlay_hints():map("<leader>th")
                require("snacks.toggle").indent():map("<leader>ti")
                require("snacks.toggle").dim():map("<leader>td")

                require("snacks.toggle").option("wrap", { name = "Wrap" }):map("<leader>tw")
                require("snacks.toggle").line_number():map("<leader>tl")
                require("snacks.toggle").option("conceallevel",
                    { off = 0, on = vim.o.conceallevel > 0 and vim.o.conceallevel or 2 }):map("<leader>th")

                require("snacks.toggle").zen():map("<leader>tx")
                require("snacks.toggle").zoom():map("<leader>tz")

                require("snacks.toggle").diagnostics():map("<leader>tD")
                require("snacks.toggle").treesitter():map("<leader>tT")
                require("snacks.toggle").option("background", { off = "light", on = "dark", name = "Dark Background" })
                    :map("<leader>tB")
            end,
        })
    end,
}
