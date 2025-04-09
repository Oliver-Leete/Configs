return {
    "chrisgrieser/nvim-genghis",
    opts = {
        trashCmd = function()
            if jit.os == "Windows" then return "trash" end
            return "trash-put"
        end,
    },
    keys = {
        { ",rn", function() require("genghis").moveSelectionToNewFile() end, mode = { "x" }, desc = "Extract to a new file" },
    },
}
