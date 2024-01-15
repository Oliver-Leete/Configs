local left = ""
local right = ""
local leftc = ""
local rightc = ""
local leftend = "▐"
local rightend = "▌"

Tabline = function()
    local tabline = ""
    local count = #vim.api.nvim_list_tabpages()
    for i, num in pairs(vim.api.nvim_list_tabpages()) do
        local tabname = vim.t[num].tabname or vim.fn.getcwd(-1, i):match(".*/(.-)$")

        local current_tab_num = vim.fn.tabpagenr()
        local is_active = i == current_tab_num

        local hl = is_active and "%#TabLineActive#" or "%#TabLine#"
        local hlre
        local hlle
        local ls = ""
        local rs = ""

        if is_active then
            ls = i == 1 and leftend or right
            rs = i == count and rightend or left
            hlle = i == 1 and "%#TabLineActiveEnds#" or "%#TabLineActiveMids#"
            hlre = i == count and "%#TabLineActiveEnds#" or "%#TabLineActiveMids#"
        else
            if i == 1 then
                ls = leftend
            elseif i < current_tab_num then
                ls = rightc
            end
            if i == count then
                rs = rightend
            elseif i > current_tab_num then
                rs = leftc
            end
            hlle = i == 1 and "%#TabLineEnds#" or "%#TabLineMids#"
            hlre = i == count and "%#TabLineEnds#" or "%#TabLineMids#"
        end

        tabline = tabline .. "%" .. i .. "T"
        tabline = tabline .. hlle .. ls
        tabline = tabline .. hl .. " " .. tabname .. " "
        tabline = tabline .. hlre .. rs
    end
    tabline = "%=" .. tabline .. "%="
    local dapstats = require("dap").status()
    if dapstats ~= "" then
        tabline = "                           " .. tabline .. require("dapui.controls").controls()
    end
    return tabline
end

vim.go.tabline = "%{%v:lua.Tabline()%}"
