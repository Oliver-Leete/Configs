vim.bo.textwidth = 100

vim.b[0].upafunc = function()
    local test_results = vim.fn.systemlist([[rg --json '\\input\{]] .. vim.fn.expand("%:r") .. [[(\.tex)?}']])
    for _, result in pairs(test_results) do
        result = vim.json.decode(result)
        if result and result.type == "match" then
            local file = result.data.path.text
            local linenr = result.data.line_number
            local colnr
            if result.data.submatch and result.data.submatch[1] then
                colnr = result.data.submatch[1].start
            else
                colnr = 0
            end
            vim.cmd.edit({ args = { file } })
            vim.api.nvim_win_set_cursor(0, { linenr, colnr + 7 })
        end
    end
end
