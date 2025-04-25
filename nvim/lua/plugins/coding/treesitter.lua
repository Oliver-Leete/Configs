---@module "lazy"
---@type LazySpec
return {
    {
        "nvim-treesitter/nvim-treesitter",
        build = { ":TSInstall all", ":TSUpdate all", ":TSUninstall comment" },
        opts = {
            indent = { enable = false },
            highlight = {
                enable = true,
                addditional_vim_regex_highlighting = false,
                disable = function(lang) return lang == "tex" or lang == "latex" end,
            },
            query_linter = {
                enable = true,
                use_virtual_text = true,
                lint_events = { "BufWrite", "CursorHold" },
            },
        },
        opts_extend = { "ensure_installed" },
        main = "nvim-treesitter.configs",
    },
    -- TODO: Add symbol swapping
    -- TODO: Make JKHL auto enter visual mode
    {
        "gsuuon/tshjkl.nvim",
        opts = {
            select_current_node = true,
            keymaps = {
                toggle = ",z",
                toggle_outer = ",Z",
                toggle_named = "Z",
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
    {
        "Wansmer/sibling-swap.nvim",
        requires = { "nvim-treesitter/nvim-treesitter" },
        opts = {
            use_default_keymaps = false,
        },
        keys = {
            { "(", function() require("sibling-swap").swap_with_left() end, desc = "Swap with left" },
            { ")", function() require("sibling-swap").swap_with_right() end, desc = "Swap with right" },
        },
    },
    {
        "CKolkey/ts-node-action",
        dependencies = { "tpope/vim-repeat" },
        opts = function()
            return {
                julia = require("ts-node-action.filetypes.julia"),
            }
        end,
        keys = {
            { ",n", function() require("ts-node-action").node_action() end, desc = "Node action" },
        },
    },
}
