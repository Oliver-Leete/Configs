function Tabline()
    local tabline = ""
    for i, num in pairs(vim.api.nvim_list_tabpages()) do
        local tabname = vim.t[num].tabname or ("Tab " .. num)

        local is_active = i == vim.fn.tabpagenr()

        local hl = is_active and "%#TabLineActive#" or "%#TabLine#"
        local hle = is_active and "%#TabLineActiveEnds#" or "%#TabLineEnds#"

        tabline = tabline .. "%" .. i .. "T"
        tabline = tabline .. hle .. ""
        tabline = tabline .. hl .. " " .. tabname .. " "
        tabline = tabline .. hle .. ""
        tabline = tabline .. "%" .. i .. "x" .. hle
    end
    local dapstats = require("dap").status()
    if dapstats ~= "" then
        tabline = tabline .. "%=" .. require("dapui").controls()
    end
    return tabline
end

vim.go.tabline = "%{%v:lua.Tabline()%}"
