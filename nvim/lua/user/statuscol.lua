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

vim.api.nvim_set_hl(0, "CursorGitSignsAdd", { fg = Ct.vcs.added, bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorGitSignsChange", { fg = Ct.vcs.changed, bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorGitSignsChangedelete", { fg = Ct.vcs.changed, bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorGitSignsDelete", { fg = Ct.vcs.removed, bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorGitSignsTopdelete", { fg = Ct.vcs.removed, bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorGitSignsUntracked", { fg = Ct.vcs.added, bg = Ct.ui.bg_p2 })

vim.api.nvim_set_hl(0, "CursorGitSignsStagedAdd", { fg = "#3b4a35", bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorGitSignsStagedChange", { fg = "#6e5230", bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorGitSignsStagedChangedelete", { fg = "#6e5230", bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorGitSignsStagedDelete", { fg = "#612021", bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorGitSignsStagedTopdelete", { fg = "#612021", bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorGitSignsStagedUntracked", { fg = "#3b4a35", bg = Ct.ui.bg_p2 })

vim.api.nvim_set_hl(0, "CursorTodoSignFIX", { fg = "#DC2626", bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorTodoSignTODO", { fg = "#2563EB", bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorTodoSignHACK", { fg = "#FBBF24", bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorTodoSignWARN", { fg = "#FBBF24", bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorTodoSignPERF", { fg = "#7C3AED", bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorTodoSignNOTE", { fg = "#10B981", bg = Ct.ui.bg_p2 })

vim.api.nvim_set_hl(0, "CursorDiagnosticSignError", { fg = "#E82424", bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorDiagnosticSignWarn", { fg = "#FF9E3B", bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorDiagnosticSignInfo", { fg = "#658494", bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorDiagnosticSignHint", { fg = "#6A9589", bg = Ct.ui.bg_p2 })

vim.api.nvim_set_hl(0, "CursorNeotestPassed", { fg = "#96F291", bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorNeotestFailed", { fg = "#F70067", bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorNeotestRunning", { fg = "#FFEC63", bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorNeotestSkipped", { fg = "#00f1f5", bg = Ct.ui.bg_p2 })
vim.api.nvim_set_hl(0, "CursorLineSign", { bg = Ct.ui.bg_p2 })

local make_sep = function(args)
    local hl_prefix = args.lnum == vim.fn.line(".", args.win) and "%#Cursor" or "%#"
    local gitsigns_unstaged = vim.api.nvim_get_namespaces().gitsigns_extmark_signs_
    local sign
    if gitsigns_unstaged then
        sign = vim.api.nvim_buf_get_extmarks(args.buf, gitsigns_unstaged, { args.lnum - 1, 0 }, { args.lnum - 1, -1 },
            { details = true })
    end
    local gitsigns_staged = vim.api.nvim_get_namespaces().gitsigns_extmark_signs_staged
    local signstaged
    if gitsigns_staged then
        signstaged = vim.api.nvim_buf_get_extmarks(args.buf, gitsigns_staged, { args.lnum - 1, 0 },
            { args.lnum - 1, -1 }, { details = true })
    end
    local text = "│"
    local name = "LineSep"
    Sign = sign
    if sign and #sign >= 1 and sign[1][4].sign_hl_group then
        name = sign[1][4].sign_hl_group
        text = gitsigns[name]
    elseif signstaged and #signstaged >= 1 and signstaged[1][4].sign_hl_group then
        name = signstaged[1][4].sign_hl_group
    end
    local hl = hl_prefix .. name .. "#"
    return hl .. text
end

 -- TODO : fix this
local builtin = require("statuscol.builtin")
require("statuscol").setup({
    setopt = true,
    thousands = false,
    relculright = false,
    ft_ignore = nil,
    bt_ignore = { "terminal" },
    segments = {
        {
            sign = {
                namespace = {
                    "dap_breakpoints",
                    "neotest.*",
                    "vim.lsp.*",
                },
                name = {
                    ".*",
                    "todo.*",
                },
                fillchar = " ",
                colwidth = 2,
                maxwidth = 1,
            },
            click = "v:lua.ScSa",
        },
        {
            text = { builtin.lnumfunc },
            click = "v:lua.ScLa",
        },
        {
            text = {
                function(args) return make_sep(args) end,
            },
            click = "v:lua.ScSa",
        },
    },
    clickmod = "c",
})
