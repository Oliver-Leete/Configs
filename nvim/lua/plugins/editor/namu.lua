local namu_defaults = {
    display = {
        format = "tree_guides",
    },
    window = {
        relative = "win"
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
            watchtower = {
                options = vim.tbl_deep_extend("force", namu_defaults, {}),
            },
            workspace = {
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
            { "<leader>::", function() require("namu.namu_symbols").show() end, desc = "Symbols (buffer)" },
            { "<leader>:;", function() require("namu.namu_watchtower").show() end, desc = "Symbols (open)" },
            { "<leader>:<c-s-;>", function() require("namu.namu_workspace").show() end, desc = "Symbols (workspace)" },
            {
                "<leader>:d",
                function() require("namu.namu_diagnostics").show_current_diagnostics() end,
                desc = "Diagnostics (buffer)",
            },
            {
                "<leader>:D",
                function() require("namu.namu_diagnostics").show_buffer_diagnostics() end,
                desc = "Diagnostics (open)",
            },
            {
                "<leader>:<c-d>",
                function() require("namu.namu_diagnostics").show_workspace_diagnostics() end,
                desc = "Diagnostics (open)",
            },
            {
                "<leader>:c",
                function() require("namu.namu_callhierarchy").show_incoming_calls() end,
                desc = "Calls (incoming)",
            },
            {
                "<leader>:C",
                function() require("namu.namu_callhierarchy").show_outgoing_calls() end,
                desc = "Calls (outgoing)",
            },
            {
                "<leader>:<c-C>",
                function() require("namu.namu_callhierarchy").show_both_calls() end,
                desc = "Calls (both)",
            },
        },
    },
}
