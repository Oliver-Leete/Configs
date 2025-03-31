return {
    "folke/which-key.nvim",
    opts = {
        preset = "helix",
        triggers = {
            { "<auto>", mode = "nxso" },
            { "v",      mode = { "n", "x" } }
        },
    },
    init = function ()
        require("which-key").add({
            {"<leader>d", group = "Debugger"},
            {"<leader>g", group = "Git"},
            {"<leader>t", group = "Toggles"},
        })
    end
}
