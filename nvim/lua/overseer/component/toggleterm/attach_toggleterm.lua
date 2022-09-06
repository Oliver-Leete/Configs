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
    params = {
        goto_bottom = {
            desc = "If the terminal should jump to the bottom",
            type = "boolean",
            defualt = true,
            optional = true,
        },
        num = {
            desc = "What harpoon number to use",
            type = "integer",
            defualt = 2,
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

                if params.num then
                    task.toggleterm:set_harp(params.num)
                end

                if params.hide then
                    task.toggleterm:toggle()
                    require("toggleterm.ui").goto_previous()
                elseif params.goto_prev then
                    require("toggleterm.ui").goto_previous()
                end
            end,
            on_restart = function(_, task)
                task.toggleterm:shutdown()
            end,
            on_dispose = function(_, task)
                if task.toggleterm then
                    task.toggleterm:shutdown()

                    local task_list = require("overseer.task_list").list_tasks()
                    local prev_task = task_list[#task_list - 1]
                    prev_task.toggleterm:set_harp(2)
                end
            end
        }
    end
}
