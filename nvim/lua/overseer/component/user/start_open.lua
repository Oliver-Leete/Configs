local overseer = require("overseer")

return {
    desc = "Open in split on task start",
    editable = false,
    serializable = true,
    params = {},
    constructor = function()
        return {
            on_start = function(_, task)
                overseer.run_action(task, "open vsplit")
            end,
        }
    end
}
