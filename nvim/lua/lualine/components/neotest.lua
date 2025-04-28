-- ## Usage
--
--   require("lualine").setup({
--     sections = {
--       lualine_x = { "neotest" },
--     },
--   })
--
--   Or with options:
--   require("lualine").setup({
--     sections = {
--       lualine_x = { {"neotest", colored = false} },
--     },
--   })
--
-- ## Options
--
-- *colored* (default: true)
--   Color the task icons.
--
-- *symbols*
--   Mapping of task status to symbol representation
--
-- *global* (default: false)
--   If the status line should show all tests or just the buffer local ones

local M = require("lualine.component"):extend()

local States = {
    Total = "total",
    Passed = "passed",
    Failed = "failed",
    Skipped = "skipped",
    Running = "running",
}

local make_init = function()
    return {
        [States.Total] = 0,
        [States.Passed] = 0,
        [States.Failed] = 0,
        [States.Skipped] = 0,
        [States.Running] = 0,
    }
end

function M:init(options)
    M.super.init(self, options)

    if self.options.global then
        self.options.buffer = nil
    else
        self.options.buffer = 0
    end

    if self.options.colored == nil then self.options.colored = true end

    local utils = require("lualine.utils.utils")
    local conf_hl = require("neotest.config").highlights
    local conf_icons = require("neotest.config").icons

    local default_icons = {
        [States.Total] = "󰤑",
        [States.Passed] = conf_icons.passed,
        [States.Failed] = conf_icons.failed,
        [States.Skipped] = conf_icons.skipped,
        [States.Running] = conf_icons.running,
    }

    local hl_groups = {
        [States.Total] = conf_hl.test,
        [States.Passed] = conf_hl.passed,
        [States.Failed] = conf_hl.failed,
        [States.Skipped] = conf_hl.skipped,
        [States.Running] = conf_hl.running,
    }

    self.symbols = vim.tbl_extend("keep", self.options.symbols or {}, default_icons)

    self.highlight_groups = {}
    if self.options.colored then
        for k, v in pairs(hl_groups) do
            local color = { fg = utils.extract_color_from_hllist("fg", { v }, "") }
            self.highlight_groups[k] = self:create_hl(color, k)
        end
    end
end

function M:update_status()
    local tbl_lreduce = require("user.utils").tbl_lreduce
    local nts = require("neotest").state

    local adapters = nts.adapter_ids()
    if #adapters <= 0 then return "" end

    ---@type neotest.state.StatusCounts[]
    local states = vim.tbl_map(
        function(adapter) return nts.status_counts(adapter, { buffer = self.options.buffer }) end,
        adapters
    )

    ---@type neotest.state.StatusCounts
    local statuses = tbl_lreduce(function(left, right) return left + right end, states, make_init())

    local pieces = {}
    for _, state in pairs(States) do
        local status = statuses[state]
        if status and status > 0 and self.symbols[state] then
            if self.options.colored then
                local hl_start = self:format_hl(self.highlight_groups[state])
                table.insert(pieces, string.format("%s%s %s", hl_start, self.symbols[state], status))
            else
                table.insert(pieces, string.format("%s %s", self.symbols[state], status))
            end
        end
    end
    if #pieces > 0 then return table.concat(pieces, " ") end
end

return M
