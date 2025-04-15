---@module "lazy"
---@type LazySpec
return {
    -- TODO: Remove one of these
    {
        "aaronik/Treewalker.nvim",
        enabled = false,
        opts = {
            highlight = true,
            highlight_duration = 250,
            highlight_group = "CursorLine",
        },
        keys = {
            { "<C-k>", "<cmd>Treewalker Up<cr>", silent = true, mode = { "n", "v" } },
            { "<C-j>", "<cmd>Treewalker Down<cr>", silent = true, mode = { "n", "v" } },
            { "<C-h>", "<cmd>Treewalker Left<cr>", silent = true, mode = { "n", "v" } },
            { "<C-l>", "<cmd>Treewalker Right<cr>", silent = true, mode = { "n", "v" } },

            { "<C-S-k>", "<cmd>Treewalker SwapUp<cr>", silent = true },
            { "<C-S-j>", "<cmd>Treewalker SwapDown<cr>", silent = true },
            { "<C-S-h>", "<cmd>Treewalker SwapLeft<cr>", silent = true },
            { "<C-S-l>", "<cmd>Treewalker SwapRight<cr>", silent = true },
        },
    },
    {
        "gsuuon/tshjkl.nvim",
        opts = {
            select_current_node = true,
            keymaps = {
                toggle = ",z",
                toggle_outer = ",Z",
                toggle_named = "Z"
            },
            marks = {
                parent = {
                    virt_text = { { "h", "ModeMsg" } },
                    virt_text_pos = "overlay",
                },
                child = {
                    virt_text = { { "l", "ModeMsg" } },
                    virt_text_pos = "overlay",
                },
                prev = {
                    virt_text = { { "k", "ModeMsg" } },
                    virt_text_pos = "overlay",
                },
                next = {
                    virt_text = { { "j", "ModeMsg" } },
                    virt_text_pos = "overlay",
                },
            },
            binds = function(bind, tshjkl)
                bind("<Esc>", function() tshjkl.exit(true) end)

                bind("q", function() tshjkl.exit(true) end)

                bind("t", function() vim.notify(tshjkl.current_node():type()) end)
            end,
        },
    },
}
