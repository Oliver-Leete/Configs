Tabline = function()
    local tabline = ""
    local count = #vim.api.nvim_list_tabpages()
    for i, num in pairs(vim.api.nvim_list_tabpages()) do
        local tabname = vim.t[num].tabname or vim.fn.getcwd(-1, i):match(".*/(.-)$")

        local is_active = i == vim.fn.tabpagenr()

        local hl = is_active and "%#TabLineActive#" or "%#TabLine#"
        local hle = is_active and "%#TabLineActiveEnds#" or "%#TabLineEnds#"
        local ls = i == 1 and "" or ""
        local rs = i == count and "" or ""

        tabline = tabline .. "%" .. i .. "T"
        tabline = tabline .. hle .. ls
        tabline = tabline .. hl .. " " .. tabname .. " "
        tabline = tabline .. hle .. rs
        tabline = tabline .. "%" .. i .. "x" .. hle
    end
    tabline = "%=" .. tabline .. "%="
    local dapstats = require("dap").status()
    if dapstats ~= "" then
        tabline = "                           " .. tabline .. require("dapui.controls").controls()
    end
    return tabline
end

vim.go.tabline = "%{%v:lua.Tabline()%}"
