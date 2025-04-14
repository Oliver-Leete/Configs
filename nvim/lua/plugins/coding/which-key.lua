---@module "lazy"
---@type LazySpec
return {
    "folke/which-key.nvim",
    opts = {
        preset = "helix",
        triggers = {
            { "<auto>", mode = "nxso" },
            { "v",      mode = { "n", "x" } }
        },
        plugins = {
            presets = {
                nav = false,
            }
        }
    },
    init = function()
        require("which-key").add({
            { "<leader>?", group = "Information", icon = " " },

            { "<leader>?c", icon = " " },
            { "<leader>?n", icon = " " },
            { "<leader>?l", "<cmd>LspInfo<cr>", desc = "Lsp info", icon = " " },
            { "<leader>?p", "<cmd>Lazy<cr>", desc = "Lazy info" },
            { "<leader>?m", "<cmd>Mason<cr>", desc = "Mason info", icon = " " },
            { "<leader>?t", group = "Treesitter", icon = " " },
            { "<leader>?tm", "<cmd>TSModuleInfo<cr>", desc = "Treesitter module info", icon = "󰕳 " },
            { "<leader>?tc", "<cmd>TSConfigInfo<cr>", desc = "Treesitter config info", icon = " " },
            { "<leader>?ti", "<cmd>TSInstallInfo<cr>", desc = "Treesitter install info", icon = " " },

            { "<leader>d", group = "Debugger", icon = " " },
            { "<leader>g", group = "Git" },
            { "<leader>n", group = "Tasks", icon = " " },
            { "<leader>r", group = "Repl", icon = " " },
            { "<leader>s", group = "UI" },
            { "<leader>t", group = "Terminals", icon = " " },
            { "<leader>u", group = "Tests" },
            { "<leader>x", group = "Compiler Explorer", icon = " " },
            { "<leader>z", group = "Toggles" },

            { "<leader>f", icon = "󰈞 " },
            { "<leader>F", icon = " " },
            { "<leader>w", icon = " " },
            { "<leader>W", icon = "󰑑 " },

            { "<leader>p", icon = " " },

            { "<leader>/", group = "Files" },
            { "<leader>/p", icon = "󰝒 " },
            { "<leader>/r", icon = "󱇧 " },
            { "<leader>/c", icon = "󰬲 " },
            { "<leader>/m", icon = "󰈪 " },
            { "<leader>/M", icon = "󰈪 " },
            { "<leader>/d", icon = "󰮘 " },

            { "<leader>:", icon = " " },
            { "]:", icon = " " },
            { "[:", icon = " " },

            { ",", group = "Editing" },
            { ",f", group = "Formatting" },
            { ",i", group = "Ignoring", icon = " " },
            { ",r", group = "Refactoring" },
            { ",l", group = "Logging", icon = "󰹈 " },

            { ",j", icon = "󰤻 " },
            { ",J", icon = "󰤻 " },
            { ",t", icon = "󱇂 ", desc = "Alignment" },
            { ",s", icon = "󰒺 ", desc = "Sort" },
            { ",=", icon = " ", desc = "Evaluate" },
            { ",n", icon = "󱘎 " },

            { ",c", icon = "󰆂 " },
            { ",C", icon = "󰆃 " },
            { ",o", icon = "󰧤 " },
            { ",O", icon = "󰧢 " },
            { ",b", icon = "󱀢 " },
            { ",a", icon = "󰆆 " },
        })
    end
}
