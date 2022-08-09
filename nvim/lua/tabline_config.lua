function Tabline()
    local tabline = ""
    for index = 1, vim.fn.tabpagenr('$') do
        local winnr = vim.fn.tabpagewinnr(index)
        local buflist = vim.fn.tabpagebuflist(index)
        local bufnr = buflist[winnr]
        local bufname = Get_unique_bufname(bufnr)

        local is_active = index == vim.fn.tabpagenr()
        local before_active = index < vim.fn.tabpagenr()
        local div = before_active and "" or ""

        mode = "Tab"
        local hl = is_active and "%#WinBar" .. mode .. "#" or "%#WinBarInactive#"
        local hle = is_active and "%#WinBar" .. mode .. "Ends#" or "%#WinBarInactiveEnds#"
        local hlme = is_active and "%#WinBar" .. mode .. "MidEnds#" or "%#WinBarInactiveMidEnds#"

        tabline = tabline .. "%" .. index .. "T"

        if index == 1 then
            tabline = tabline .. hle .. ""
        elseif not is_active and before_active then
            tabline = tabline .. hl .. div
        else
            tabline = tabline .. hlme .. ""
        end

        tabline = tabline .. hl .. " " .. bufname .. " "

        if index == vim.fn.tabpagenr('$') then
            tabline = tabline .. hle .. ""
        elseif not is_active and not before_active then
            tabline = tabline .. hl .. div
        else
            tabline = tabline .. hlme .. ""
        end

        tabline = tabline .. "%" .. index .. "x"
    end
    return tabline
end

vim.go.tabline = "%{%v:lua.Tabline()%}"
