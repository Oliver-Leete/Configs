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
    },
    init = function()
        require("which-key").add({
            { "<leader>g", group = "Git" },
            { "<leader>t", group = "Terminals", icon = " " },
            { "<leader>z", group = "Toggles" },
            { "<leader>n", group = "Tasks", icon = " " },
            { "<leader>d", group = "Debugger", icon = " " },
            { "<leader>?", group = "Information", icon = " " },
            { "<leader>x", group = "Compiler Explorer", icon = " "},
            { "<leader>s", group = "UI"},
            { "<leader>r", group = "Repl", icon = " " },

            { ",", group = "Editing" },
            { ",r", group = "Refactoring" },
            { ",f", group = "Formatting" },
            { ",i", group = "Ignoring", icon = " " },
        })
    end
}
