return {
    "stevearc/oil.nvim",
    opts = {
        keymaps = {
            ["<esc>"] = "actions.close",
        },
        delete_to_trash = true,
        lsp_file_methods = {
            enabled = false,
        },
    },
    keys = {
        { "-", "<CMD>Oil<CR>", desc = "Open parent directory" }
    },
    init = function()
        vim.api.nvim_create_autocmd("User", {
            pattern = "OilActionsPost",
            callback = function(event)
                if event.data.actions.type == "move" then
                    require("snacks.rename").on_rename_file(event.data.actions.src_url, event.data.actions.dest_url)
                end
            end,
        })
    end
}
