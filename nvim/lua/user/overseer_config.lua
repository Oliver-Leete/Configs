local overseer = require("overseer")
local STATUS = require("overseer.constants").STATUS
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
            ["o"] = "<cmd>OverseerQuickAction open in toggleterm<cr>",
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
            { "user.attach_toggleterm", hide = true },
            "display_duration",
        },
    },
    actions = {
        ["open vsplit"] = false,
        ["open hsplit"] = false,
        ["set loclist diagnostics"] = false,
        ["toggle open"] = {
            desc = "open in toggleterm",
            run = function(task)
                if task.toggleterm then
                    task.toggleterm:toggle()
                else
                    local bufnr = task.strategy.bufnr
                    task.toggleterm = require("toggleterm.terminal").Terminal:new({ bufnr = bufnr, jobname = task.name })
                    task:add_components({ "user.attach_toggleterm" })
                    task.toggleterm:toggle()
                    task.toggleterm:__resurrect()
                end
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
    templates = { "builtin", "user", "julia" }
})
