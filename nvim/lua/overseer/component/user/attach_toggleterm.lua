Terminal = require("toggleterm.terminal").Terminal

return {
    desc = "Attach a toggleterm to the task",
    editable = false,
    serializable = true,
    params = {
        goto_bottom = {
            desc = "If the terminal should jump to the bottom",
            type = "boolean",
            defualt = true,
            optional = true,
        },
        goto_prev = {
            desc = "Should you return to where you were?",
            type = "boolean",
            defualt = false,
            optional = true,
        },
        hide = {
            desc = "Should it start big???",
            type = "boolean",
            defualt = false,
            optional = true,
        },
    },
    constructor = function(params)
        return {
            on_init = function()
            end,
            on_start = function(_, task)
                Task = task
                if task.toggleterm then
                    task.toggleterm:shutdown()
                end
                local bufnr = task.strategy.bufnr
                local name = task.test_name or task.name

                task.toggleterm = Terminal:new({ bufnr = bufnr, jobname = name })

                task.toggleterm:toggle()
                task.toggleterm:__resurrect()

                require("toggleterm.ui").scroll_to_bottom()

                if params.hide then
                    task.toggleterm:toggle()
                    require("toggleterm.ui").goto_previous()
                elseif params.goto_prev then
                    require("toggleterm.ui").goto_previous()
                end
                OTerm = task.toggleterm
            end,
            on_restart = function(_, task)
                task.toggleterm:shutdown()
            end,
            on_dispose = function(_, task)
                if task.toggleterm then
                    task.toggleterm:shutdown()
                end
            end
        }
    end
}
