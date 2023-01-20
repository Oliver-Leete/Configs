local builtin = require("statuscol.builtin")

local function diagnostic_click(args)
    if args.button == "l" then
        vim.diagnostic.open_float({ border = Border, scope = "line", source = "always" })
    elseif args.button == "m" then
        vim.lsp.buf.code_action()
    end
end

require("statuscol").setup({
    separator              = "",
    -- Click actions
    Lnum                   = builtin.lnum_click,
    FoldPlus               = builtin.foldplus_click,
    FoldMinus              = builtin.foldminus_click,
    FoldEmpty              = builtin.foldempty_click,
    DapBreakpointRejected  = builtin.toggle_breakpoint,
    DapBreakpoint          = builtin.toggle_breakpoint,
    DapBreakpointCondition = builtin.toggle_breakpoint,
    DiagnosticSignError    = diagnostic_click,
    DiagnosticSignHint     = diagnostic_click,
    DiagnosticSignInfo     = diagnostic_click,
    DiagnosticSignWarn     = diagnostic_click,
    GitSignsTopdelete      = builtin.gitsigns_click,
    GitSignsUntracked      = builtin.gitsigns_click,
    GitSignsAdd            = builtin.gitsigns_click,
    GitSignsChangedelete   = builtin.gitsigns_click,
    GitSignsDelete         = builtin.gitsigns_click,
})

local gitconfig = require("gitsigns.config").config

local gitsigns = {
    GitSignsAdd          = "▐",
    GitSignsChange       = "▐",
    GitSignsDelete       = "▐",
    GitSignsTopdelete    = "▐",
    GitSignsChangedelete = "▐",
    GitSignsUntracked    = "░",
}

ScGs = function()
    local bufnr = vim.api.nvim_get_current_buf()
    local sign = vim.fn.sign_getplaced(bufnr, { lnum = vim.v.lnum, group = "gitsigns_vimfn_signs_" })[1].signs
    local signstaged = vim.fn.sign_getplaced(bufnr, { lnum = vim.v.lnum, group = "gitsigns_vimfn_signs_staged" })[1].signs
    local hl
    local text = "▕"
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
    return hl .. vim.v.lnum
end

vim.o.statuscolumn = "%{%v:lua.ScEs()%}%=%{%v:lua.ScMn()%}%{%v:lua.ScGs()%}"

local colGroup = vim.api.nvim_create_augroup("colGroup", {})
for _, filetype in pairs(vim.tbl_keys(require("user.myfuncs").special_types)) do
    if filetype ~= "" then
        vim.api.nvim_create_autocmd(
            "FileType",
            {
                pattern = filetype,
                callback = function() vim.wo.statuscolumn = "%C" end,
                group = colGroup,
            }
        )
    end
end
