local overseer = require("overseer")
local STATUS = require("overseer.constants").STATUS
local util = require("overseer.util")

local function close_task(bufnr)
    local oldwin = vim.tbl_filter(function(t) return (t.strategy.bufnr == bufnr) end, overseer.list_tasks())[1]
    for _, winnr in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
        if vim.api.nvim_win_is_valid(winnr) then
            local winbufnr = vim.api.nvim_win_get_buf(winnr)
            if vim.bo[winbufnr].filetype == "OverseerPanelTask" then
                if oldwin then
                    vim.api.nvim_win_close(winnr, true)
                end
            end
        end
    end
end

overseer.setup({
    strategy = "terminal",
    form = { border = Border, win_opts = { winblend = 0, }, },
    task_editor = { border = Border, win_opts = { winblend = 0, }, },
    task_win = { border = Border, win_opts = { winblend = 0, }, },
    confirm = { border = Border, win_opts = { winblend = 0, }, },
    task_list = {
        separator = "",
        bindings = {
            ["?"] = "ShowHelp",
            ["<CR>"] = "RunAction",
            ["<C-e>"] = "Edit",
            ["o"] = function() vim.cmd.wincmd({ args = { "l" } }) end,
            ["p"] = "TogglePreview",
            ["<C-l>"] = "IncreaseDetail",
            ["<C-h>"] = "DecreaseDetail",
            ["L"] = "IncreaseAllDetail",
            ["H"] = "DecreaseAllDetail",
            ["["] = "PrevTask",
            ["]"] = "NextTask",
            ["{"] = "",
            ["}"] = "",
            ["<C-v>"] = "",
            ["<C-f>"] = "OpenFloat",
            ["<esc>"] = function() overseer.close() end,
        },
        direction = "bottom",
        max_height = { 40, 0.2 },
        min_height = 20,
    },
    component_aliases = {
        default_neotest = {
            "on_output_summarize",
            "on_exit_set_status",
            { "on_complete_notify", system = "unfocused" },
            "on_complete_dispose",
            "unique",
            "display_duration",
        },
        default = {
            "display_duration",
            "on_output_summarize",
            "on_exit_set_status",
            { "on_complete_notify", system = "unfocused" },
            "on_complete_dispose",
        },
        always_restart = { "on_complete_restart", statuses = { STATUS.FAILURE, STATUS.SUCCESS } },
    },
    template_timeout = 5000,
    template_cache_threshold = 0,
    actions = {
        ["open hsplit"] = false,
        ["set loclist diagnostics"] = false,
        ["keep runnning"] = {
            desc = "restart the task even if it succeeds",
            run = function(task)
                task:add_components({ { "on_complete_restart", statuses = { STATUS.FAILURE, STATUS.SUCCESS } } })
                if task.status == STATUS.FAILURE or task.status == STATUS.SUCCESS then
                    task:restart()
                end
            end
        },
        ["unwatch"] = {
            desc = "stop from running on finish or file watch",
            run = function(task)
                for _, component in pairs({ "on_complete_restart", "on_complete_restart" }) do
                    if task:has_component(component) then
                        task:remove_components({ component })
                    end
                end
            end,
            condition = function(task)
                return task:has_component("on_complete_restart") or task:has_component("restart_on_save")
            end
        },
    },
    templates = { "cargo", "just", "make", "npm", "shell", "tox", "vscode", "mix", "rake", "task", "user" }
})

vim.api.nvim_set_hl(0, "OverseerTaskBorder", { link = "Normal" })
