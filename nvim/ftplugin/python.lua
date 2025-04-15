vim.bo.textwidth = 0
vim.b[0].textwidth = 0
vim.cmd([[call matchadd('TabLineSel', '\%101v', 80)]])

vim.b[0].upafunc = function()
    local path = vim.api.nvim_buf_get_name(0)
    if vim.fs.basename(path) == "__init__.py" then path = vim.fs.dirname(path) end
    path = vim.fs.dirname(path)
    for name, _ in vim.fs.dir(path) do
        if vim.fs.basename(name) == "__init__.py" then
            vim.cmd.edit({ args = { path .. "/" .. name } })
            return
        end
    end
end
