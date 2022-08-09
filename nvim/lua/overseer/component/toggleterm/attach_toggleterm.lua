Terminal = require("toggleterm.terminal").Terminal

return {
    desc = "Clean up toggleterm terminal after the task is disposed",
    editable = false,
    serializable = true,
    constructor = function()
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
                task.toggleterm:toggle()
            end,
            on_restart = function(_, task)
                task.toggleterm:shutdown()
            end,
            on_dispose = function(_, task)
                task.toggleterm:shutdown()
            end
        }
    end
}
