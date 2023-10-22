M = {}

require("grapple").setup({
    integrations = {
        resession = true
    }
})

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
        grapple = {
            enable_in_tab = true,
        },
    },
    tab_buf_filter = function(tabpage, bufnr)
        local dir = vim.fn.getcwd(-1, vim.api.nvim_tabpage_get_number(tabpage))
        return vim.startswith(vim.api.nvim_buf_get_name(bufnr), dir)
    end,
})

-- vim.api.nvim_create_autocmd("VimEnter", {
--     callback = function()
--         -- Only load the session if nvim was started with no args
--         if vim.fn.argc(-1) == 0 then
--             -- Save these to a different directory, so our manual sessions don't get polluted
--             resession.load(vim.fn.getcwd(), { dir = "dirsession", silence_errors = true })
--         end
--     end,
-- })

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

-- Always save a special session named "last"
vim.api.nvim_create_autocmd("VimLeavePre", {
    group = projects,
    callback = function()
        if not vim.g.dont_save_session then
            resession.save("last", { dir = workspace_dir })
        end
    end,
})

return M
