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

vim.api.nvim_create_autocmd("VimLeavePre", {
    callback = function()
        resession.save_tab(vim.fn.getcwd(), { notify = false })
    end,
})
vim.api.nvim_create_autocmd("TabLeave", {
    callback = function()
        resession.save_tab(vim.fn.getcwd(), { notify = false })
    end,
})

-- Always save a special session named "last"
vim.api.nvim_create_autocmd("VimLeavePre", {
    callback = function()
        resession.save("last")
    end,
})
