M = {}

local resession = require("resession")
resession.setup({
    autosave = {
        enabled = true,
        interval = 60,
        notify = false,
    },
    extensions = {
        overseer = {
            filter = function(task)
                return task.metadata.run_on_open
            end
        },
        quickfix = {
            enable_in_tab = true,
        },
    },
    tab_buf_filter = function(tabpage, bufnr)
        local dir = vim.fn.getcwd(-1, vim.api.nvim_tabpage_get_number(tabpage))
        return vim.startswith(vim.api.nvim_buf_get_name(bufnr), dir)
    end,
})

local projects = vim.api.nvim_create_augroup("projects", {})
local workspace_dir = "session/" .. vim.fn.system("wmctrl -d | grep '*' | awk '{print $NF}'"):gsub("\n", "")

M.load_session = function()
    local list = resession.list({ dir = workspace_dir })
    if #list > 0 then
        vim.ui.select(
            list,
            {},
            function(name)
                if name then
                    resession.load(name, { dir = workspace_dir })
                end
            end
        )
    end
end

M.load_session_reset = function()
    local list = resession.list({ dir = workspace_dir })
    if #list > 0 then
        vim.ui.select(
            list,
            {},
            function(name)
                if name then
                    resession.load(name, { dir = workspace_dir, reset = true })
                end
            end
        )
    end
end

vim.api.nvim_create_user_command("LoadSession", M.load_session, {})

M.delete_session = function()
    local list = resession.list({ dir = workspace_dir })
    if #list > 0 then
        vim.ui.select(
            list,
            {},
            function(name)
                if name then
                    resession.delete(name, { dir = workspace_dir })
                end
            end
        )
    end
end

M.save_session = function()
    vim.ui.input({ prompt = "Session Name:" }, function(name)
        require("resession").save_tab(name, { dir = workspace_dir })
    end)
end

vim.api.nvim_create_autocmd("VimLeavePre", {
    group = projects,
    callback = function()
        if not vim.g.dont_save_session then
            resession.save_tab(vim.fn.getcwd(), { notify = false, dir = workspace_dir })
        end
    end,
})
vim.api.nvim_create_autocmd("TabLeave", {
    group = projects,
    callback = function()
        if not vim.g.dont_save_session then
            resession.save_tab(vim.fn.getcwd(), { notify = false, dir = workspace_dir })
        end
    end,
})

return M
