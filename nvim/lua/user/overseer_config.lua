local overseer = require("overseer")
local STATUS = require("overseer.constants").STATUS

local toggleterm_if_not = function(task)
    local bufnr = task.strategy.bufnr
    task.toggleterm = require("toggleterm.terminal").Terminal:new({ bufnr = bufnr, jobname = task.name })
    task:add_components({ "user.attach_toggleterm" })
    task.toggleterm.job_id = task.strategy.chan_id
    task.toggleterm:toggle()
    task.toggleterm:__resurrect()
end

overseer.setup({
    form = { border = Border, win_opts = { winblend = 0, }, },
    task_editor = { border = Border, win_opts = { winblend = 0, }, },
    task_win = { border = Border, win_opts = { winblend = 0, }, },
    confirm = { border = Border, win_opts = { winblend = 0, }, },
    task_list = {
        bindings = {
            ["?"] = "ShowHelp",
            ["<CR>"] = "RunAction",
            ["<C-e>"] = "Edit",
            ["o"] = "Open",
            ["p"] = "TogglePreview",
            ["<C-l>"] = "IncreaseDetail",
            ["<C-h>"] = "DecreaseDetail",
            ["L"] = "IncreaseAllDetail",
            ["H"] = "DecreaseAllDetail",
            ["["] = "PrevTask",
            ["]"] = "NextTask",
            ["{"] = nil,
            ["}"] = nil,
            ["<C-v>"] = nil,
            ["<C-f>"] = nil,
        },
    },
    component_aliases = {
        default_neotest = {
            "on_output_summarize",
            "on_exit_set_status",
            { "on_complete_notify", system = "unfocused" },
            "on_complete_dispose",
            { "user.attach_toggleterm", goto_prev = true },
            "unique",
            "display_duration",
        },
        default = {
            "on_output_summarize",
            "on_exit_set_status",
            { "on_complete_notify", system = "unfocused" },
            "on_complete_dispose",
            "user.attach_toggleterm",
            "display_duration",
        },
        default_hide = {
            "on_output_summarize",
            "on_exit_set_status",
            { "on_complete_notify", system = "unfocused" },
            "on_complete_dispose",
            "display_duration",
        },
        def_dispose = {
            { "on_complete_dispose", timeout = 1 },
            "user.attach_toggleterm",
        },
        always_restart = { "on_complete_restart", statuses = { STATUS.FAILURE, STATUS.SUCCESS } },
    },
    actions = {
        ["open vsplit"] = false,
        ["open hsplit"] = false,
        ["set loclist diagnostics"] = false,
        ["open"] = {
            desc = "open in toggleterm",
            run = function(task)
                if task.toggleterm then
                    task.toggleterm:toggle()
                else
                    toggleterm_if_not(task)
                end
                OTerm = task.toggleterm
            end,
        },
        ["set as recive terminal"] = {
            desc = "set this task as the terminal to recive sent text and commands",
            run = function(task)
                if not task.toggleterm then
                    toggleterm_if_not(task)
                end
                OTerm = task.toggleterm
                STerm = task.toggleterm
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
        ["stop repeat running"] = {
            desc = "stop from running on finish or file watch",
            run = function(task)
                if task:has_component("on_complete_restart") then
                    task:remove_components({ "on_complete_restart" })
                end
                if task:has_component("restart_on_save") then
                    task:remove_components({ "restart_on_save" })
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
            end
        },
        ["restart with server"] = {
            desc = "restart the server with the task",
            run = function(task)
                local task_list = require("overseer.task_list").list_tasks()
                for _, ntask in pairs(task_list) do
                    if ntask.metadata.is_test_server then
                        ntask:restart(true)
                    end
                end
                task:restart(true)
            end,
            condition = function(task)
                local not_pend = task.status ~= STATUS.PENDING
                return task.metadata.uses_server and not_pend
            end,
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
