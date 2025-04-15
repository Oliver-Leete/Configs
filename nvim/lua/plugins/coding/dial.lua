---@module "lazy"
---@type LazySpec
return {
    "monaqa/dial.nvim",
    keys = {
        { "<c-a>", function() require("dial.map").manipulate("increment", "normal") end, desc = "Increment" },
        { "<c-r>", function() require("dial.map").manipulate("decrement", "normal") end, desc = "Decrement" },
        { "g<c-a>", function() require("dial.map").manipulate("increment", "gnormal") end, desc = "Increment (g)" },
        { "g<c-r>", function() require("dial.map").manipulate("decrement", "gnormal") end, desc = "Decrement (g)" },
        {
            "<c-a>",
            function() require("dial.map").manipulate("increment", "visual") end,
            desc = "Increment",
            mode = { "v" },
        },
        {
            "<c-r>",
            function() require("dial.map").manipulate("decrement", "visual") end,
            desc = "Decrement",
            mode = { "v" },
        },
        {
            "g<c-a>",
            function() require("dial.map").manipulate("increment", "gvisual") end,
            desc = "Increment (g)",
            mode = { "v" },
        },
        {
            "g<c-r>",
            function() require("dial.map").manipulate("decrement", "gvisual") end,
            desc = "Decrement (g)",
            mode = { "v" },
        },
        {
            "<c-f>",
            function() require("dial.map").manipulate("increment", "normal", "cycle_case") end,
            desc = "Cycle case",
        },
        {
            "<c-f>",
            function() require("dial.map").manipulate("increment", "visual", "cycle_case") end,
            desc = "Cycle case",
            mode = { "v" },
        },
    },
    opts = function()
        local augend = require("dial.augend")

        local default = {
            augend.integer.alias.decimal,
            augend.integer.alias.hex,
            augend.date.alias["%Y/%m/%d"],
            augend.date.alias["%Y-%m-%d"],
            augend.semver.alias.semver,
        }
        -- stylua: ignore
        local word_cycles = {
            { "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven", "twelve" },
            { "first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth" },
            { "january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december" },
            { "jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec" },
            { "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday" },
            { "sun", "mon", "tue", "wed", "thu", "fri", "sat" },
            { "today", "tomorrow", "yesterday" },
            { "morning", "noon", "afternoon", "evening" },
            { "day", "night" },
            { "am", "pm" },
            { "true", "false" },
            { "yes", "no" },
            { "on", "off" },
            { "up", "down" },
            { "enable", "disable" },
            { "enabled", "disabled" },
            { "set", "unset" },
            { "is", "isnot" },
            { "in", "out" },
            { "input", "output" },
            { "inner", "outer" },
            { "inside", "outside" },
            { "client", "server" },
            { "success", "failure" },
            { "successful", "failed" },
            { "verbose", "debug", "info", "warn", "error", "fatal" },
            { "remote", "local", "base" },
            { "ours", "theirs" },
            { "main", "master", "dev" },
            { "development", "production", "test" },
            { "accept", "decline" },
            { "add", "remove" },
            { "allow", "deny" },
            { "buy", "sell" },
            { "close", "open" },
            { "compress", "decompress" },
            { "create", "destroy" },
            { "enter", "exit" },
            { "expand", "collapse" },
            { "fail", "succeed" },
            { "import", "export" },
            { "increase", "decrease" },
            { "join", "leave" },
            { "lock", "unlock" },
            { "push", "pull" },
            { "read", "write" },
            { "receive", "send" },
            { "save", "discard" },
            { "show", "hide" },
            { "start", "stop" },
            { "win", "lose" },
            { "max", "min" },
            { "maximum", "minimum" },
            { "maximal", "minimal" },
            { "maximize", "minimize" },
            { "upper", "lower" },
            { "top", "bottom" },
            { "above", "below" },
            { "forward", "backward" },
            { "right", "middle", "left" },
            { "next", "previous" },
            { "first", "last" },
            { "begin", "end" },
            { "before", "after" },
            { "red", "green", "blue", "black", "white", "gray", "brown", "yellow", "orange", "purple" },
            { "cyan", "magenta", "yellow" },
            { "small", "medium", "large" },
            { "hot", "warm", "cold" },
            { "few", "many" },
            { "much", "little" },
            { "more", "less" },
            { "good", "bad" },
            { "better", "worse" },
            { "best", "worst" },
            { "bigger", "smaller" },
            { "cheap", "expensive" },
            { "clean", "dirty" },
            { "clear", "unclear" },
            { "detailed", "brief" },
            { "early", "late" },
            { "easy", "difficult" },
            { "fast", "slow" },
            { "gain", "loss" },
            { "high", "low" },
            { "internal", "external" },
            { "light", "dark" },
            { "major", "minor" },
            { "manual", "automatic" },
            { "new", "old" },
            { "normal", "abnormal" },
            { "now", "later" },
            { "optional", "mandatory" },
            { "permanent", "temporary" },
            { "private", "public" },
            { "quiet", "loud" },
            { "rich", "poor" },
            { "simple", "complex" },
            { "strong", "weak" },
            { "bit", "byte", "kilobyte", "megabyte", "gigabyte", "terabyte" },
            { "north", "east", "south", "west" },
            { "horizontal", "vertical" },
        }
        local word_augends = vim.tbl_map(function(word_cycle)
            return augend.constant.new({
                elements = word_cycle,
                word = false, -- #TODO: add a pattern that finds subwords so that this can be turned back on
                cyclic = true,
                preserve_case = true,
            })
        end, word_cycles)
        vim.list_extend(default, word_augends)

        -- stylua: ignore
        local symbol_cycles = {
            { "&&", "||" },
            { "&", "|" },
            { "+", "-" },
            { "++", "--" },
            { "==", "!=" },
            { "=~", "!~" },
            { "<", ">" },
            { "<=", ">=" },
            { ">>", "<<" },
            { "∈", "∉", "∋", "∌" },
            { "∪", "∩" },
            { "⊓", "⊔" },
            { "∨", "∧" },
            { "⊂", "⊃", "⊊", "⊋" },
            { "∘", "⨟" },
            { "|>", ".|>" },
            { ":<", ":>" },
            { "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹", "⁰" },
            { "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉", "₀" },
            { "ᵢ", "ⱼ", "ₖ" },
            { "ₙ", "ₘ", "ₗ" },
        }
        local symbol_augends = vim.tbl_map(
            function(symbol_cycle) return augend.constant.new({ elements = symbol_cycle, word = false, cyclic = true }) end,
            symbol_cycles
        )
        vim.list_extend(default, symbol_augends)

        return {
            groups = {
                default = default,
                cycle_case = {
                    augend.case.new({
                        types = { "PascalCase", "camelCase", "snake_case", "kebab-case", "SCREAMING_SNAKE_CASE" },
                        cyclic = true,
                    }),
                },
            },
            dials_by_ft = {
                default = default,
                markdown = {
                    augend.constant.new({ elements = { "[ ]", "[/]", "[x]" }, word = false, cyclic = true }),
                    augend.misc.alias.markdown_header,
                },
                gitrebase = {
                    augend.constant.new({
                        elements = {
                            "pick",
                            "reword",
                            "edit",
                            "squash",
                            "fixup",
                            "exec",
                            "break",
                            "drop",
                        },
                        cyclic = true,
                        word = true,
                    }),
                },
                python = {},
                julia = {},
                lua = {
                    augend.constant.new({ elements = { "local ", "M." }, word = false, cyclic = true }),
                    augend.paren.new({
                        patterns = {
                            { "[[", "]]" },
                            { "[=[", "]=]" },
                            { "[==[", "]==]" },
                            { "[===[", "]===]" },
                        },
                        nested = true,
                        cyclic = false,
                    }),
                },
                tex = {
                    augend.paren.new({ patterns = { { "$", "$" }, { "\\(", "\\)" } }, nested = false, cyclic = true }),
                },
            },
        }
    end,
    config = function(_, opts)
        -- copy defaults to each group
        require("dial.config").augends:register_group(opts.groups)

        for _, group in pairs(opts.dials_by_ft) do
            vim.list_extend(group, opts.groups.default)
        end
        require("dial.config").augends:on_filetype(opts.dials_by_ft)
    end,
}
