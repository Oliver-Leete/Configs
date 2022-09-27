require('swenv').setup({
    venvs_path = vim.fn.expand('~/.local/python_venvs/')
})
vim.b[0].localCommands = function()
    return {
        { source = "python", name = "change venv", func = function() require('swenv.api').pick_venv() end },
    }
end
