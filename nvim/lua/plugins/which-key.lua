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
            { "<leader>t", group = "Tests", icon = "󰙨 " },
            { "<leader>z", group = "Toggles" },
            { "<leader>o", group = "Tasks", icon = " " },
            { "<leader>d", group = "Debugger", icon = " " },
        })
    end
}
