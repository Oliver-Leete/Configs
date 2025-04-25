local namu_defaults = {
    display = {
        format = "tree_guides",
    },
    hierarchical_mode = true,
    preserve_hierarchy = true,
    movement = {
        next = { "<down>" },
        previous = { "<up>" },
        close = { "<esc>" },
        select = { "<cr>" },
        delete_word = { "<c-w>" },
        clear_line = { "<c-u>" },
    },
    multiselect = {
        keymaps = {
            toggle = "<tab>",
            select_all = "<C-a>",
            clear_all = "<C-l>",
            untoggle = "<s-Tab>",
        },
    },
    row_position = "top10_right",
    custom_keymaps = {
        horizontal_split = {
            keys = { "<c-x>" },
        },
    },
    focus_current_symbol = true,
    auto_select = false,
    initially_hidden = false,
    actions = {
        close_on_yank = false,
        close_on_delete = false,
    },
    icon = "󱠦 ",
}
---@module "lazy"
---@type LazySpec
return {
    {
        "folke/which-key.nvim",
        optional = true,
        opts = {
            to_add = {
                namu = {
                    { "<leader>:", icon = " " },
                },
            },
        },
    },
    {
        "bassamsdata/namu.nvim",
        ---@module "namu"
        opts = {
            namu_symbols = {
                options = vim.tbl_deep_extend("force", namu_defaults, {}),
            },
            diagnostics = {
                options = vim.tbl_deep_extend("force", namu_defaults, {
                    window = {
                        title_prefix = " > ",
                        min_width = 25,
                    },
                    icons = {
                        Error = " ",
                        Warn = " ",
                        Info = " ",
                        Hint = "󰅽 ",
                    },
                }),
            },
            workspace = {
                options = vim.tbl_deep_extend("force", namu_defaults, {}),
            },
            callhierarchy = {
                options = vim.tbl_deep_extend("force", namu_defaults, {
                    sort_by_nesting_depth = true,
                    call_hierarchy = {
                        max_depth = 4,
                        max_depth_limit = 4,
                        show_cycles = true,
                    },
                }),
            },
        },
        cmd = { "Namu" },
        keys = {
            { "<leader>:", "<cmd>Namu watchtower<cr>", desc = "Lsp Symbols" },
        },
    },
}
