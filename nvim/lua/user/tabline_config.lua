function Tabline()
    local tabline = ""
    for index = 1, vim.fn.tabpagenr('$') do
        local tabname = vim.t[index].tabname or ("Tab " .. index)

        local is_active = index == vim.fn.tabpagenr()

        local hl = is_active and "%#TabLineActive#" or "%#TabLine#"
        local hle = is_active and "%#TabLineActiveEnds#" or "%#TabLineEnds#"

        tabline = tabline .. "%" .. index .. "T"
        tabline = tabline .. hle .. ""
        tabline = tabline .. hl .. " " .. tabname .. " "
        tabline = tabline .. hle .. ""
        tabline = tabline .. "%" .. index .. "x" .. hle
    end
    local dapstats = require("dap").status()
    if dapstats ~= "" then
        tabline = tabline .. "%=" .. require("dapui").controls()
    end
    return tabline
end

vim.go.tabline = "%{%v:lua.Tabline()%}"
