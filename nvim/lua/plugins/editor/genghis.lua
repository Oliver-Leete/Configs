return {
    "chrisgrieser/nvim-genghis",
    opts = {
        trashCmd = function()
            if jit.os == "Windows" then return "trash" end
            return "trash-put"
        end,
    },
    keys = {
        { ",rf", function() require("genghis").moveSelectionToNewFile() end, mode = { "x" }, desc = "Move to new file" },
    },
}
