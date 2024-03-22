local gitsigns = {
    GitSignsAdd          = "┃",
    GitSignsChange       = "┃",
    GitSignsDelete       = "╽",
    GitSignsTopdelete    = "╿",
    GitSignsChangedelete = "┃",
    GitSignsUntracked    = "┋",
}

local make_sep = function(args)
    local hl_prefix = "%#"
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
