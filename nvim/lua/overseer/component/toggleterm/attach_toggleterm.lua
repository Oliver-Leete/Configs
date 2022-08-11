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
                local open = AnyTermOpen()
                task.toggleterm = Terminal:new({ bufnr = bufnr, jobname = name })
                task.toggleterm:toggle()
                task.toggleterm:__resurrect()
                require("toggleterm.ui").goto_previous()
                task.toggleterm:set_harp(2)
                if not open then
                    task.toggleterm:close()
                end
            end,
            on_restart = function(_, task)
                task.toggleterm:shutdown()
            end,
            on_dispose = function(_, task)
                task.toggleterm:shutdown()

                task.toggleterm:set_harp(2)
            end
        }
    end
}
