local M = {}

---@param increment boolean
---@param g? boolean
function M.dial(increment, g)
    local mode = vim.fn.mode(true)
    -- Use visual commands for VISUAL 'v', VISUAL LINE 'V' and VISUAL BLOCK '\22'
    local is_visual = mode == "v" or mode == "V" or mode == "\22"
    local func = (increment and "inc" or "dec") .. (g and "_g" or "_") .. (is_visual and "visual" or "normal")
    local group = vim.g.dials_by_ft[vim.bo.filetype] or "default"
    return require("dial.map")[func](group)
end

return {
    "monaqa/dial.nvim",
    keys = {
        { "<c-a>",  function() return M.dial(true) end,        expr = true, desc = "Increment", mode = { "n", "v" } },
        { "<c-r>",  function() return M.dial(false) end,       expr = true, desc = "Decrement", mode = { "n", "v" } },
        { "g<c-a>", function() return M.dial(true, true) end,  expr = true, desc = "Increment", mode = { "n", "v" } },
        { "g<c-r>", function() return M.dial(false, true) end, expr = true, desc = "Decrement", mode = { "n", "v" } },
    },
    opts = function()
        local augend = require("dial.augend")

        local ordinal_numbers = augend.constant.new({
            elements = {
                "first",
                "second",
                "third",
                "fourth",
                "fifth",
                "sixth",
                "seventh",
                "eighth",
                "ninth",
                "tenth",
            },
            word = false,
            cyclic = true,
        })
        local logical_alias = augend.constant.new({ elements = { "&&", "||" }, word = false, cyclic = true })
        local logical_word_alias = augend.constant.new({ elements = { "and", "or" }, word = true, cyclic = true })
        local capitalized_boolean = augend.constant.new({ elements = { "True", "False", }, word = true, cyclic = true })

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
            },
            groups = {
                default = {
                    augend.integer.alias.decimal,
                    augend.integer.alias.decimal_int,
                    augend.integer.alias.hex,
                    augend.date.alias["%Y/%m/%d"],
                    ordinal_numbers,
                    capitalized_boolean,
                    augend.constant.alias.bool,
                    logical_alias,
                    logical_word_alias,
                    augend.semver.alias.semver,
                },
                markdown = {
                    augend.constant.new({ elements = { "[ ]", "[x]" }, word = false, cyclic = true }),
                    augend.misc.alias.markdown_header,
                },
                gitrebase = {
                    rebase_commands,
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
