local is_cursorline = function()
    return vim.v.lnum == vim.fn.line(".")
end

local gitsigns = {
    GitSignsAdd          = "┃",
    GitSignsChange       = "┃",
    GitSignsDelete       = "╽",
    GitSignsTopdelete    = "╿",
    GitSignsChangedelete = "┃",
    GitSignsUntracked    = "┋",
}

vim.api.nvim_set_hl(0, "CursorGitSignsAdd", { fg = Tc.autumnGreen, bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "CursorGitSignsChange", { fg = Tc.autumnYellow, bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "CursorGitSignsChangedelete", { fg = Tc.autumnYellow, bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "CursorGitSignsDelete", { fg = Tc.autumnRed, bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "CursorGitSignsTopdelete", { fg = Tc.autumnRed, bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "CursorGitSignsUntracked", { fg = Tc.autumnGreen, bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "CursorTodoSignFIX", { fg = "#DC2626", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "CursorTodoSignTODO", { fg = "#2563EB", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "CursorTodoSignHACK", { fg = "#FBBF24", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "CursorTodoSignWARN", { fg = "#FBBF24", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "CursorTodoSignPERF", { fg = "#7C3AED", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "CursorTodoSignNOTE", { fg = "#10B981", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "CursorDiagnosticSignError", { fg = "#E82424", bg = "#363646" })
vim.api.nvim_set_hl(0, "CursorDiagnosticSignWarn", { fg = "#FF9E3B", bg = "#363646" })
vim.api.nvim_set_hl(0, "CursorDiagnosticSignInfo", { fg = "#658494", bg = "#363646" })
vim.api.nvim_set_hl(0, "CursorDiagnosticSignHint", { fg = "#6A9589", bg = "#363646" })
vim.api.nvim_set_hl(0, "CursorNeotestPassed", { fg = "#96F291", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "CursorNeotestFailed", { fg = "#F70067", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "CursorNeotestRunning", { fg = "#FFEC63", bg = Tc.sumiInk3 })
vim.api.nvim_set_hl(0, "CursorNeotestSkipped", { fg = "#00f1f5", bg = Tc.sumiInk3 })



local make_sep = function(hl_prefix)
    local bufnr = vim.api.nvim_get_current_buf()
    local sign = vim.fn.sign_getplaced(bufnr, { lnum = vim.v.lnum, group = "gitsigns_vimfn_signs_" })[1].signs
    local signstaged = vim.fn.sign_getplaced(bufnr, { lnum = vim.v.lnum, group = "gitsigns_vimfn_signs_staged" })[1]
        .signs
    local text = "│"
    local name
    if #sign >= 1 and sign[1].name then
        name = sign[1].name
        text = gitsigns[name]
    elseif #signstaged >= 1 and signstaged[1].name then
        name = signstaged[1].name
    else
        name = "LineNr"
    end
    local hl = hl_prefix .. name .. "#"
    return hl .. text
end

local get_signs = function()
    local buf = vim.api.nvim_win_get_buf(vim.g.statusline_winid or 0)
    return vim.tbl_map(function(sign)
            local ret = vim.fn.sign_getdefined(sign.name)[1]
            ret.priority = sign.priority
            return ret
        end, vim.fn.sign_getplaced(buf, { group = "*", lnum = vim.v.lnum })[1].signs)
end


Normal_StatusCol = function()
    local cursorline = is_cursorline()
    local hl_prefix = cursorline and "%#Cursor" or "%#"

    local signs = vim.tbl_filter(function(s) return not s.name:find("GitSign") end, get_signs("*"))
    table.sort(signs, function(a, b)
        return a.priority > b.priority
    end)

    local num = vim.v.lnum
    local num_len = string.len(num)
    local total_len = num_len < 4 and 6 or num_len + 2

    local num_out = ""
    local sign_limit
    if cursorline or (#signs * 2) <= (total_len - num_len) then
        local hlnum = hl_prefix .. "LineNr#"
        num_out =  hlnum .. num
        sign_limit = math.floor((total_len - num_len) / 2)
    else
        sign_limit = math.floor(total_len / 2)
        num_len = 0
    end

    local sign_out = ""
    for i=1,sign_limit do
        if signs[i] then
            local sign_hl = hl_prefix .. signs[i].texthl .. "#"
            sign_out = sign_out .. sign_hl .. signs[i].text
        end
    end

    local odd_space = total_len % 2 == 1 and 1 or 0
    local sign_len = math.min(sign_limit, #signs) * 2
    local spacing = odd_space + total_len - (num_len + sign_len)
    local spacing_out = string.rep(" ", spacing)

    return sign_out .. spacing_out .. num_out .. make_sep(hl_prefix)
end
