Terminal = require("toggleterm.terminal").Terminal
local function anyTermOpen()
    local term_list = require("toggleterm.terminal").get_all()
    local open
    for _, term in pairs(term_list) do
        if term:is_open() then
            open = true
            break
        end
    end
    return open
end

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

                local open = anyTermOpen()
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

                local task_list = require("overseer.task_list").list_tasks()
                local prev_task = task_list[#task_list-1]
                prev_task.toggleterm:set_harp(2)
            end
        }
    end
}
