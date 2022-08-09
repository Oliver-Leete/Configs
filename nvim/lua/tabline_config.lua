function Tabline()
    local tabline = ""
    for index = 1, vim.fn.tabpagenr('$') do
        local winnr = vim.fn.tabpagewinnr(index)
        local buflist = vim.fn.tabpagebuflist(index)
        local bufnr = buflist[winnr]
        local bufname = Get_unique_bufname(bufnr)

        local is_active = index == vim.fn.tabpagenr()

        local hl = is_active and "%#TabLineActive#" or "%#TabLine#"
        local hle = is_active and "%#TabLineActiveEnds#" or "%#TabLineEnds#"

        tabline = tabline .. "%" .. index .. "T"
        tabline = tabline .. hl .. ""
        tabline = tabline .. hl .. " " .. bufname .. " "
        tabline = tabline .. hl .. ""
        tabline = tabline .. "%" .. index .. "x" .. hle
    end
    return tabline
end

vim.go.tabline = "%{%v:lua.Tabline()%}"
