return {
    "folke/snacks.nvim",
    config = function()
        require("snacks").setup({
            picker = {
                layout = { preset = "ivy" },
                win = {
                    input = {
                        keys = {
                            ["<c-u>"] = "",
                        }
                    }
                },
            }
        })

        Map({ "n" }, "<leader>f", function()
            local results = require("telescope.utils").get_os_command_output({ "git", "rev-parse", "--git-dir" })
            if results == nil then
                vim.notify("Something went wrong", vim.log.levels.WARN)
            elseif results[1] then
                Snacks.picker.git_files()
            else
                Snacks.picker.files()
            end
        end, { desc = "Find files" })

        Map({ "n" }, "<leader>F", function() Snacks.picker.resume() end,
            { desc = "Resume last picker" })

        Map("n", "<leader>w", function() Snacks.picker.lsp_workspace_symbols() end,
            { desc = "Workspace symbols" })
        Map("n", "<leader>W", function() Snacks.picker.grep() end, { desc = "Grep" })
    end
}
