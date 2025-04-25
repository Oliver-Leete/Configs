---@module "lazy"
---@type LazySpec
return {
    {
        "folke/which-key.nvim",
        optional = true,
        opts = {
            to_add = {
                files = {
                    { "<leader>/", group = "Files" },
                    { "<leader>/n", icon = "󰝒 " },
                    { "<leader>/r", icon = "󱇧 " },
                    { "<leader>/c", icon = "󰬲 " },
                    { "<leader>/m", icon = "󰈪 " },
                    { "<leader>/M", icon = "󰈪 " },
                    { "<leader>/t", icon = "󰮘 " },
                    { "<leader>/d", icon = "󰮘 " },
                },
            },
        },
    },
    {
        "chrisgrieser/nvim-genghis",
        opts = {
            trashCmd = function()
                if jit.os == "Windows" then return "trash" end
                return "trash-put"
            end,
        },
        keys = {
            { "<leader>/n", function() require("genghis").createNewFile() end, desc = "New file" },
            { "<leader>/r", function() require("genghis").renameFile() end, desc = "Rename file" },
            { "<leader>/c", function() require("genghis").duplicateFile() end, desc = "Copy file" },

            { "<leader>/m", function() require("genghis").moveAndRenameFile() end, desc = "Move file" },
            { "<leader>/M", function() require("genghis").moveToFolderInCwd() end, desc = "Move file to subdir" },

            { "<leader>/d", function() require("genghis").trashFile() end, desc = "Trash file" },
            { "<leader>/t", function() require("genghis").trashFile() end, desc = "Trash file" },
        },
    },
}
