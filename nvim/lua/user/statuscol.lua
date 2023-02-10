local gitsigns = {
    GitSignsAdd          = "┃",
    GitSignsChange       = "┃",
    GitSignsDelete       = "╽",
    GitSignsTopdelete    = "╿",
    GitSignsChangedelete = "┃",
    GitSignsUntracked    = "┋",
}

ScGs = function()
    local bufnr = vim.api.nvim_get_current_buf()
    local sign = vim.fn.sign_getplaced(bufnr, { lnum = vim.v.lnum, group = "gitsigns_vimfn_signs_" })[1].signs
    local signstaged = vim.fn.sign_getplaced(bufnr, { lnum = vim.v.lnum, group = "gitsigns_vimfn_signs_staged" })[1].signs
    local hl
    local text = "│"
    if #sign >= 1 and sign[1].name then
        local name = sign[1].name
        hl = "%#" .. name .. (vim.v.lnum == vim.fn.line(".") and "Cur" or "") .. "#"
        text = gitsigns[name]
    elseif #signstaged >= 1 and signstaged[1].name then
        local name = signstaged[1].name
        hl = "%#" .. name .. (vim.v.lnum == vim.fn.line(".") and "Cur" or "") .. "#"
    else
        hl = "%#" .. (vim.v.lnum == vim.fn.line(".") and "CursorLineNr" or "LineNr") .. "#"
    end
    return hl .. text
end

local get_signs = function()
    local buf = vim.api.nvim_win_get_buf(vim.g.statusline_winid or 0)
    return vim.tbl_map(function(sign)
        local ret = vim.fn.sign_getdefined(sign.name)[1]
        ret.priority = sign.priority
        Ret = ret
        return ret
    end, vim.fn.sign_getplaced(buf, { group = "*", lnum = vim.v.lnum })[1].signs)
end

vim.api.nvim_set_hl(0, "TodoSignFIXCur", { fg = "#DC2626", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "TodoSignTODOCur", { fg = "#2563EB", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "TodoSignHACKCur", { fg = "#FBBF24", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "TodoSignWARNCur", { fg = "#FBBF24", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "TodoSignPERFCur", { fg = "#7C3AED", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "TodoSignNOTECur", { fg = "#10B981", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "DiagnosticSignErrorCur", { fg = "#E82424", bg = "#363646" })
vim.api.nvim_set_hl(0, "DiagnosticSignWarnCur", { fg = "#FF9E3B", bg = "#363646" })
vim.api.nvim_set_hl(0, "DiagnosticSignInfoCur", { fg = "#658494", bg = "#363646" })
vim.api.nvim_set_hl(0, "DiagnosticSignHintCur", { fg = "#6A9589", bg = "#363646" })
vim.api.nvim_set_hl(0, "NeotestPassedCur", { fg = "#96F291", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "NeotestFailedCur", { fg = "#F70067", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "NeotestRunningCur", { fg = "#FFEC63", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "NeotestSkippedCur", { fg = "#00f1f5", bg = Tc.sumiInk3 })

ScEs = function()
    local sign
    local signs = get_signs("*")
    signs = vim.tbl_filter(function(s) return not s.name:find("GitSign") and s.name ~= "LightBulbSign" end, signs)
    if not signs and not signs[1] then return "   " end
    table.sort(signs, function(a, b)
        return a.priority < b.priority
    end)
    sign = signs[1]
    if not sign then return "   " end
    local hl = "%#" .. sign.texthl .. (vim.v.lnum == vim.fn.line(".") and "Cur#" or "#")
    return hl .. sign.text
end

ScMn = function()
    local hl = "LineNr#"
    hl = (vim.v.lnum == vim.fn.line(".") and "%#Cursor" or "%#") .. hl
    local num = vim.v.lnum
    local len = string.len(num)
    if len < 3 then
        num = string.rep(" ", 3-string.len(vim.v.lnum)) .. vim.v.lnum
    end
    return hl .. num
    -- return hl .. vim.v.lnum
end
