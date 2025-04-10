---@param increment boolean
---@param g? boolean
local function dial(increment, g)
    local mode = vim.fn.mode(true)
    -- Use visual commands for VISUAL "v", VISUAL LINE "V" and VISUAL BLOCK "\22"
    local is_visual = mode == "v" or mode == "V" or mode == "\22"
    local func = (increment and "inc" or "dec") .. (g and "_g" or "_") .. (is_visual and "visual" or "normal")
    local group = vim.g.dials_by_ft[vim.bo.filetype] or "default"
    return require("dial.map")[func](group)
end

---@module "lazy"
---@type LazySpec
return {
    "monaqa/dial.nvim",
    keys = {
        { "<c-a>",  function() return dial(true) end,        expr = true, desc = "Increment", mode = { "n", "v" } },
        { "<c-r>",  function() return dial(false) end,       expr = true, desc = "Decrement", mode = { "n", "v" } },
        { "g<c-a>", function() return dial(true, true) end,  expr = true, desc = "Increment", mode = { "n", "v" } },
        { "g<c-r>", function() return dial(false, true) end, expr = true, desc = "Decrement", mode = { "n", "v" } },
    },
    opts = function()
        local augend = require("dial.augend")

        local default = {
            augend.integer.alias.decimal_int,
            augend.integer.alias.hex,
            augend.date.alias["%Y/%m/%d"],
            augend.date.alias["%Y-%m-%d"],
            augend.semver.alias.semver,
        }
        local word_cycles = {
            { "zero",        "one",        "two",       "three",     "four",     "five",    "six",     "seven",  "eight",     "nine",    "ten",      "eleven",  "twelve" },
            { "Zero",        "One",        "Two",       "Three",     "Four",     "Five",    "Six",     "Seven",  "Eight",     "Nine",    "Ten",      "Eleven",  "Twelve" },
            { "first",       "second",     "third",     "fourth",    "fifth",    "sixth",   "seventh", "eighth", "ninth",     "tenth",   "eleventh", "twelfth" },
            { "First",       "Second",     "Third",     "Fourth",    "Fifth",    "Sixth",   "Seventh", "Eighth", "Ninth",     "Tenth",   "Eleventh", "Twelfth" },
            { "january",     "february",   "march",     "april",     "may",      "june",    "july",    "august", "september", "october", "november", "december" },
            { "January",     "February",   "March",     "April",     "May",      "June",    "July",    "August", "September", "October", "November", "December" },
            { "jan",         "feb",        "mar",       "apr",       "may",      "jun",     "jul",     "aug",    "sep",       "oct",     "nov",      "dec" },
            { "Jan",         "Feb",        "Mar",       "Apr",       "May",      "Jun",     "Jul",     "Aug",    "Sep",       "Oct",     "Nov",      "Dec" },
            { "sunday",      "monday",     "tuesday",   "wednesday", "thursday", "friday",  "saturday" },
            { "Sunday",      "Monday",     "Tuesday",   "Wednesday", "Thursday", "Friday",  "Saturday" },
            { "sun",         "mon",        "tue",       "wed",       "thu",      "fri",     "sat" },
            { "Sun",         "Mon",        "Tue",       "Wed",       "Thu",      "Fri",     "Sat" },
            { "today",       "tomorrow",   "yesterday" },
            { "Today",       "Tomorrow",   "Yesterday" },
            { "morning",     "noon",       "afternoon", "evening" },
            { "Morning",     "Noon",       "Afternoon", "Evening" },
            { "day",         "night" },
            { "Day",         "Night" },
            { "am",          "pm" },
            { "AM",          "PM" },
            { "true",        "false" },
            { "True",        "False" },
            { "TRUE",        "FALSE" },
            { "yes",         "no" },
            { "Yes",         "No" },
            { "YES",         "NO" },
            { "on",          "off" },
            { "On",          "Off" },
            { "ON",          "OFF" },
            { "up",          "down" },
            { "Up",          "Down" },
            { "UP",          "DOWN" },
            { "enable",      "disable" },
            { "Enable",      "Disable" },
            { "enabled",     "disabled" },
            { "Enabled",     "Disabled" },
            { "set",         "unset" },
            { "is",          "isnot" },
            { "in",          "out" },
            { "In",          "Out" },
            { "input",       "output" },
            { "Input",       "Output" },
            { "inner",       "outer" },
            { "Inner",       "Outer" },
            { "inside",      "outside" },
            { "Inside",      "Outside" },
            { "client",      "server" },
            { "Client",      "Server" },
            { "success",     "failure" },
            { "Success",     "Failure" },
            { "successful",  "failed" },
            { "Successful",  "Failed" },
            { "verbose",     "debug",      "info",      "warn",      "error",    "fatal" },
            { "remote",      "local",      "base" },
            { "REMOTE",      "LOCAL",      "BASE" },
            { "ours",        "theirs" },
            { "main",        "master",     "dev" },
            { "development", "production", "test" },
            { "accept",      "decline" },
            { "Accept",      "Decline" },
            { "add",         "remove" },
            { "Add",         "Remove" },
            { "allow",       "deny" },
            { "Allow",       "Deny" },
            { "buy",         "sell" },
            { "Buy",         "Sell" },
            { "close",       "open" },
            { "Close",       "Open" },
            { "compress",    "decompress" },
            { "Compress",    "Decompress" },
            { "create",      "destroy" },
            { "Create",      "Destroy" },
            { "enter",       "exit" },
            { "Enter",       "Exit" },
            { "expand",      "collapse" },
            { "Expand",      "Collapse" },
            { "fail",        "succeed" },
            { "Fail",        "Succeed" },
            { "import",      "export" },
            { "Import",      "Export" },
            { "increase",    "decrease" },
            { "Increase",    "Decrease" },
            { "join",        "leave" },
            { "Join",        "Leave" },
            { "lock",        "unlock" },
            { "Lock",        "Unlock" },
            { "push",        "pull" },
            { "Push",        "Pull" },
            { "read",        "write" },
            { "Read",        "Write" },
            { "receive",     "send" },
            { "Receive",     "Send" },
            { "save",        "discard" },
            { "Save",        "Discard" },
            { "show",        "hide" },
            { "Show",        "Hide" },
            { "start",       "stop" },
            { "Start",       "Stop" },
            { "win",         "lose" },
            { "Win",         "Lose" },
            { "max",         "min" },
            { "Max",         "Min" },
            { "maximum",     "minimum" },
            { "Maximum",     "Minimum" },
            { "maximal",     "minimal" },
            { "Maximal",     "Minimal" },
            { "maximize",    "minimize" },
            { "Maximize",    "Minimize" },
            { "upper",       "lower" },
            { "Upper",       "Lower" },
            { "top",         "bottom" },
            { "Top",         "Bottom" },
            { "above",       "below" },
            { "Above",       "Below" },
            { "forward",     "backward" },
            { "Forward",     "Backward" },
            { "right",       "middle",     "left" },
            { "Right",       "Middle",     "Left" },
            { "next",        "previous" },
            { "Next",        "Previous" },
            { "first",       "last" },
            { "First",       "Last" },
            { "begin",       "end" },
            { "Begin",       "End" },
            { "before",      "after" },
            { "Before",      "After" },
            { "red",         "green",      "blue",      "black",     "white",    "gray",    "brown",   "yellow", "orange",    "purple" },
            { "Red",         "Green",      "Blue",      "Black",     "White",    "Gray",    "Brown",   "Yellow", "Orange",    "Purple" },
            { "cyan",        "magenta",    "yellow" },
            { "Cyan",        "Magenta",    "Yellow" },
            { "small",       "medium",     "large" },
            { "Small",       "Medium",     "Large" },
            { "hot",         "warm",       "cold" },
            { "Hot",         "Warm",       "Cold" },
            { "few",         "many" },
            { "Few",         "Many" },
            { "much",        "little" },
            { "Much",        "Little" },
            { "more",        "less" },
            { "More",        "Less" },
            { "good",        "bad" },
            { "Good",        "Bad" },
            { "better",      "worse" },
            { "Better",      "Worse" },
            { "best",        "worst" },
            { "Best",        "Worst" },
            { "bigger",      "smaller" },
            { "Bigger",      "Smaller" },
            { "cheap",       "expensive" },
            { "Cheap",       "Expensive" },
            { "clean",       "dirty" },
            { "Clean",       "Dirty" },
            { "clear",       "unclear" },
            { "Clear",       "Unclear" },
            { "detailed",    "brief" },
            { "Detailed",    "Brief" },
            { "early",       "late" },
            { "Early",       "Late" },
            { "easy",        "difficult" },
            { "Easy",        "Difficult" },
            { "fast",        "slow" },
            { "Fast",        "Slow" },
            { "gain",        "loss" },
            { "Gain",        "Loss" },
            { "high",        "low" },
            { "High",        "Low" },
            { "internal",    "external" },
            { "Internal",    "External" },
            { "light",       "dark" },
            { "Light",       "Dark" },
            { "major",       "minor" },
            { "Major",       "Minor" },
            { "manual",      "automatic" },
            { "Manual",      "Automatic" },
            { "new",         "old" },
            { "New",         "Old" },
            { "normal",      "abnormal" },
            { "Normal",      "Abnormal" },
            { "now",         "later" },
            { "Now",         "Later" },
            { "optional",    "mandatory" },
            { "Optional",    "Mandatory" },
            { "permanent",   "temporary" },
            { "Permanent",   "Temporary" },
            { "private",     "public" },
            { "Private",     "Public" },
            { "quiet",       "loud" },
            { "Quiet",       "Loud" },
            { "rich",        "poor" },
            { "Rich",        "Poor" },
            { "simple",      "complex" },
            { "Simple",      "Complex" },
            { "strong",      "weak" },
            { "Strong",      "Weak" },
            { "bit",         "byte",       "kilobyte",  "megabyte",  "gigabyte", "terabyte" },
            { "Bit",         "Byte",       "Kilobyte",  "Megabyte",  "Gigabyte", "Terabyte" },
            { "north",       "east",       "south",     "west" },
            { "North",       "East",       "South",     "West" },
            { "horizontal",  "vertical" },
            { "Horizontal",  "Vertical" },
        }
        for _, words in ipairs(word_cycles) do
            default[#default + 1] = augend.constant.new({ elements = words, word = true, cyclic = true })
        end
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
            { "¹", "²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹", "⁰" },
            { "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉", "₀" },
            { "ᵢ", "ⱼ", "ₖ" },
            { "ₙ", "ₘ", "ₗ" },
        }
        for _, symbols in ipairs(symbol_cycles) do
            default[#default + 1] = augend.constant.new({ elements = symbols, word = false, cyclic = true })
        end

        local rebase_commands = augend.constant.new({
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
        })

        return {
            dials_by_ft = {
                markdown = "markdown",
                gitrebase = "gitrebase",
                python = "python",
                julia = "julia",
            },
            groups = {
                default = default,
                markdown = {
                    augend.constant.new({ elements = { "[ ]", "[x]" }, word = false, cyclic = true }),
                    augend.misc.alias.markdown_header,
                },
                gitrebase = {
                    rebase_commands,
                },
                python = {
                },
                julia = {
                },
            },
        }
    end,
    config = function(_, opts)
        -- copy defaults to each group
        for name, group in pairs(opts.groups) do
            if name ~= "default" then
                vim.list_extend(group, opts.groups.default)
            end
        end
        require("dial.config").augends:register_group(opts.groups)
        vim.g.dials_by_ft = opts.dials_by_ft
    end,
}
