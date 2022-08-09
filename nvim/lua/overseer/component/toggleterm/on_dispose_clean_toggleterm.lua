return {
    desc = "Clean up toggleterm terminal after the task is disposed",
    editable = false,
    serializable = true,
    constructor = function()
        return {
            on_init = function()
            end,
            on_dispose = function(_, task)
                task.toggleterm:shutdown()
            end
        }
    end
}
