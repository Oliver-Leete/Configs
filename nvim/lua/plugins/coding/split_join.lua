---@module "lazy"
---@type LazySpec
return {
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
    {
        "wurli/split.nvim",
        enabled = false, -- #TODO: Make this actually work how I want it to, then reenable
        opts = function()
            return {
                keymaps = {
                    [",k"] = {
                        pattern = ",",
                        operator_pending = true,
                        interactive = false,
                    },
                    [",kk"] = {
                        pattern = ",",
                        operator_pending = false,
                        interactive = false,
                    },
                    [",K"] = {
                        pattern = ",",
                        operator_pending = true,
                        interactive = true,
                    },
                    [",KK"] = {
                        pattern = ",",
                        operator_pending = false,
                        interactive = true,
                    },
                },
                interactive_options = {
                    [","] = ",",
                    [";"] = ";",
                    [" "] = "%s+",
                    ["+"] = " [+-/%%] ",
                    ["<"] = {
                        pattern = "[<>=]=?",
                        break_placement = "before_pattern",
                    },
                    ["."] = {
                        pattern = "[%.?!]%s+",
                        unsplitter = " ",
                        smart_ignore = "code",
                        quote_characters = {},
                        brace_characters = {},
                    },
                    ["|"] = {
                        pattern = { "|>", "%%>%%" },
                        break_placement = "after_pattern",
                    },
                },
                keymap_defaults = {
                    pattern = ",",
                    break_placement = "after_pattern",
                    operator_pending = false,
                    transform_segments = require("split.utils").make_transformer({
                        trim_l = { "before_pattern", "on_pattern", "after_pattern" },
                        trim_r = { "before_pattern", "on_pattern", "after_pattern" },
                    }),
                    transform_separators = require("split.utils").make_transformer({
                        trim_l = { "before_pattern" },
                        trim_r = { "before_pattern", "on_pattern", "after_pattern" },
                        pad_r = { "before_pattern" },
                    }),
                    indenter = "simple",
                    unsplitter = nil,
                    interactive = false,
                    smart_ignore = "comments",
                    quote_characters = { left = { "'", '"', "`" }, right = { "'", '"', "`" } },
                    brace_characters = { left = { "(", "{", "[" }, right = { ")", "}", "]" } },
                },
                set_default_mappings = false,
            }
        end,
    },
}
-- (1, 2, 3)
