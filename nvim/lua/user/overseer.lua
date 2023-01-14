local overseer = require("overseer")
local STATUS = require("overseer.constants").STATUS

local tt = require("toggleterm.terminal")

local task_to_term = function(task)
    return vim.tbl_filter(function(term)
        return term.bufnr == task.strategy.bufnr
    end, tt.get_all(true))[1]
end

overseer.setup({
    strategy = "toggleterm",
    form = { border = Border, win_opts = { winblend = 0, }, },
    task_editor = { border = Border, win_opts = { winblend = 0, }, },
    task_win = { border = Border, win_opts = { winblend = 0, }, },
    confirm = { border = Border, win_opts = { winblend = 0, }, },
    task_list = {
        separator = "────────────────────────────────────────────────────────────────────────────────",
        bindings = {
            ["?"] = "ShowHelp",
            ["<CR>"] = "RunAction",
            ["<C-e>"] = "Edit",
            ["o"] = "Toggle",
            ["O"] = "Open",
            ["p"] = "TogglePreview",
            ["<C-l>"] = "IncreaseDetail",
            ["<C-h>"] = "DecreaseDetail",
            ["L"] = "IncreaseAllDetail",
            ["H"] = "DecreaseAllDetail",
            ["("] = "PrevTask",
            [")"] = "NextTask",
            ["["] = "",
            ["]"] = "",
            ["{"] = "",
            ["}"] = "",
            ["<C-v>"] = "",
            ["<C-f>"] = "OpenFloat",
        },
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
            "on_output_summarize",
            "on_exit_set_status",
            { "on_complete_notify", system = "unfocused" },
            "on_complete_dispose",
            "display_duration",
        },
        always_restart = { "on_complete_restart", statuses = { STATUS.FAILURE, STATUS.SUCCESS } },
    },
    template_timeout = 5000,
    template_cache_threshold = 0,
    actions = {
        toggle = {
            run = function(task)
                local term = task_to_term(task)
                term:toggle()
            end
        },
        ["open vsplit"] = false,
        ["open hsplit"] = false,
        ["set loclist diagnostics"] = false,
        ["toggle hide"] = {
            desc = "close and detach the toggleterm",
            run = function(task)
                local term = task_to_term(task)
                if term.hidden then
                    term.hidden = false
                    term:open()
                    OTerm = term
                else
                    if OTerm == term then
                        OTerm = nil
                    end
                    if STerm == term then
                        STerm = nil
                    end
                    term.hidden = true
                    term:close()
                end
            end,
        },
        ["set as recive terminal"] = {
            desc = "set this task as the terminal to recive sent text and commands",
            run = function(task)
                local term = task_to_term(task)
                term.hidden = false
                OTerm = term
                STerm = term
            end,
        },
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
        ["don't dispose"] = {
            desc = "keep the task until manually disposed",
            run = function(task)
                task:remove_components({ "on_complete_dispose" })
            end,
            condition = function(task)
                return task:has_component("on_complete_dispose")
            end
        },
        ["dump task"] = {
            desc = "save task table to DumpTask (for debugging)",
            run = function(task)
                DumpTask = task
            end,
        }
    },
    templates = { "builtin", "user" }
})

vim.api.nvim_set_hl(0, "OverseerTaskBorder", { link = "Normal" })
