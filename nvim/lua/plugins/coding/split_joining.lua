---@module "lazy"
---@type LazySpec
return {
    {
        "folke/which-key.nvim",
        optional = true,
        opts = {
            to_add = {
                ["split-joining"] = {
                    { ",j", icon = "󰤻 " },
                    { ",J", icon = "󰤻 " },
                    -- { ",k", icon = "󰡏 " },
                },
            },
        },
    },
    {
        "Wansmer/treesj",
        dev = true,
        dependencies = { "echasnovski/mini.splitjoin" },
        opts = {
            use_default_keymaps = false,
            max_join_length = 10000,
            notify = false,
            on_error = function(err_text, level, ...)
                local msg = require("treesj.notify").msg
                if
                    err_text
                    and not (
                        err_text == msg.no_ts_parser
                        or err_text == msg.no_configured_lang
                        or err_text == msg.no_configured_node
                        or err_text == msg.no_format_with
                    )
                then
                    level = level or vim.log.levels.INFO
                    vim.notify(err_text:format(...), level, { title = "TreeSJ: split/join code block " })
                else
                    require("mini.splitjoin").toggle()
                end
            end,
        },
        keys = {
            {
                ",j",
                function() require("treesj").toggle() end,
                desc = "Split/join",
                mode = { "n", "x" },
            },
        },
    },
    {
        "echasnovski/mini.splitjoin",
        opts = {
            mappings = {
                toggle = "",
            },
            detect = {
                separator = "[,;]",
            },
            split = {
                hooks_pre = {},
                hooks_post = {},
            },
            -- Join options
            join = {
                hooks_pre = {},
                hooks_post = {},
            },
        },
        keys = {
            {
                ",J",
                function() require("mini.splitjoin").toggle() end,
                desc = "Split/join (force mini)",
                mode = { "n", "x" },
            },
        },
    },
}
