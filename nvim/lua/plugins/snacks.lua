return {
    "folke/snacks.nvim",
    config = function()
        require("snacks").setup({
            picker = {
                layout = { preset = "ivy" },
                actions = require("trouble.sources.snacks").actions,
                win = {
                    input = {
                        keys = {
                            ["<c-x>"] = { "edit_split", mode = { "n", "i" }, },
                            ["<c-l>"] = { "trouble_open", mode = { "n", "i" }, },
                            ["<c-u>"] = "",
                        }
                    }
                },
            }
        })

        vim.keymap.set({ "n" }, "<leader>f", function()
            local results = require("telescope.utils").get_os_command_output({ "git", "rev-parse", "--git-dir" })
            if results == nil then
                vim.notify("Something went wrong", vim.log.levels.WARN)
            elseif results[1] then
                require("snacks.picker").git_files()
            else
                require("snacks.picker").files()
            end
        end, { desc = "Find files" })

        vim.keymap.set({ "n" }, "<leader>F", function() require("snacks.picker").resume() end,
            { desc = "Resume last picker" })

        vim.keymap.set("n", "<leader>w", function() require("snacks.picker").lsp_workspace_symbols() end,
            { desc = "Workspace symbols" })
        vim.keymap.set("n", "<leader>W", function() require("snacks.picker").grep() end, { desc = "Grep" })
    end
}
