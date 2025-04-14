---@module "lazy"
---@type LazySpec
return {
    "chrisgrieser/nvim-genghis",
    opts = {
        trashCmd = function()
            if jit.os == "Windows" then return "trash" end
            return "trash-put"
        end,
    },
    keys = {
        { "<leader>/p", function() require("genghis").createNewFile() end,          desc = "New file" },
        { "<leader>/r", function() require("genghis").renameFile() end,             desc = "Rename file" },
        { "<leader>/c", function() require("genghis").duplicateFile() end,          desc = "Copy file" },
        { "<leader>/m", function() require("genghis").moveAndRenameFile() end,      desc = "Move file" },
        { "<leader>/M", function() require("genghis").moveToFolderInCwd() end,      desc = "Move file to subdir" },
        { "<leader>/d", function() require("genghis").trashFile() end,              desc = "Trash file" },

        { ",rn",        function() require("genghis").moveSelectionToNewFile() end, desc = "Extract to a new file", mode = { "x" } },
    },
}
