----------------------------------------------------------------------------------------------------
--                      _   _   ______    ____   __      __  _____   __  __                       --
--                     | \ | | |  ____|  / __ \  \ \    / / |_   _| |  \/  |                      --
--                     |  \| | | |__    | |  | |  \ \  / /    | |   | \  / |                      --
--                     | . ` | |  __|   | |  | |   \ \/ /     | |   | |\/| |                      --
--                     | |\  | | |____  | |__| |    \  /     _| |_  | |  | |                      --
--                     |_| \_| |______|  \____/      \/     |_____| |_|  |_|                      --
--                                                                                                --
----------------------------------------------------------------------------------------------------
-- Oliver Leete <oliverleete@gmail.com>                                                            --
-- https://github.com/oliver-leete                                                                 --
----------------------------------------------------------------------------------------------------

-- Compleation (compe) and Snippet (luasnip) Setup

vim.o.completeopt = "menuone,noselect"
require("compe").setup({
    enabled = true,
    autocomplete = true,
    debug = false,
    min_length = 1,
    preselect = "enable",
    throttle_time = 80,
    source_timeout = 200,
    incomplete_delay = 400,
    max_abbr_width = 100,
    max_kind_width = 100,
    max_menu_width = 100,
    documentation = {
        border = "single",
        winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
        max_width = 120,
        min_width = 60,
        max_height = math.floor(vim.o.lines * 0.3),
        min_height = 1,
    },

    source = {
        path = true,
        buffer = true,
        calc = true,
        nvim_lsp = true,
        nvim_lua = true,
        nvim_treesitter = false,
        vsnip = false,
        luasnip = true,
        omni = {
            filetypes = { "tex" },
        },
        tabnine = true,
    },
})

-- Compe and luasnip mappings
local function replace_keycodes(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local luasnip = require("luasnip")
_G.tab_complete = function()
    if luasnip and luasnip.expand_or_jumpable() then
        return replace_keycodes("<Plug>luasnip-expand-or-jump")
    else
        return replace_keycodes("<plug>(TaboutMulti)")
    end
end

_G.s_tab_complete = function()
    if luasnip and luasnip.jumpable(-1) then
        return replace_keycodes("<Plug>luasnip-jump-prev")
    else
        return replace_keycodes("<plug>(TaboutBackMulti)")
    end
end

_G.compe_toggle = function()
    if vim.fn.pumvisible() == 1 then
        -- return replace_keycodes("<esc>:call compe#close()<cr>a")
        return replace_keycodes("<plug>(compe-close)")
    else
        return replace_keycodes("<cmd>call compe#complete()<cr>")
    end
end

_G.enter_complete = function()
    if vim.fn.pumvisible() == 1 then
        if luasnip and luasnip.expandable() then
            return replace_keycodes("<plug>luasnip-expand-snippet")
        else
            return vim.fn['compe#confirm'](require('nvim-autopairs').esc('<cr>'))
        end
    elseif luasnip and luasnip.choice_active() then
        return replace_keycodes("<plug>luasnip-next-choice")
    else
        return require('nvim-autopairs').autopairs_cr()
    end
end

vim.cmd([[
    augroup autopairs_compe
    autocmd!
    autocmd User CompeConfirmDone call v:lua.MPairs.completion_done()
    augroup end
]])

vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", { expr = true })
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", { expr = true })
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", { expr = true })
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", { expr = true })
vim.api.nvim_set_keymap("i", "<c-space>", "v:lua.compe_toggle()", {expr = true})
vim.api.nvim_set_keymap("s", "<c-space>", "v:lua.compe_toggle()", {expr = true})
vim.api.nvim_set_keymap("i", "<cr>", "v:lua.enter_complete()", { expr = true })
vim.api.nvim_set_keymap("s", "<cr>", "v:lua.enter_complete()", { expr = true })

-- AutoPairs Setup
require("nvim-autopairs").setup({
    check_ts = true,
    enable_check_bracket_line = true,
    fast_wrap = {
      map = '<C-e>',
      chars = { '{', '[', '(', '"', "'" , "`"},
      pattern = string.gsub([[ [%'%"%)%>%]%)%}%,] ]], '%s+', ''),
      end_key = 'L',
      keys = 'tnseriaodhgjplfuwybkvmcxzq',
    },
})

local Rule = require('nvim-autopairs.rule')
require("nvim-autopairs").add_rules({
    Rule('"""', '"""', 'julia'),
    Rule("$", "$", "tex"),
})

require("nvim-autopairs.completion.compe").setup({
    map_cr = false,
    map_complete = true,
})

vim.g.diagnostic_auto_popup_while_jump = 0
vim.g.diagnostic_enable_virtual_text = 0
vim.g.diagnostic_enable_underline = 0
vim.g.completion_timer_cycle = 200

require("todo-comments").setup({
    signs = true,
    keywords = {
        FIX = { icon = " ", color = "error", alt = { "FIXME", "BUG", "FIXIT", "FIX", "ISSUE" } },
        TODO = { icon = " ", color = "info" },
        HACK = { icon = " ", color = "warning", alt = { "JANK", "WORKAROUND" } },
        WARN = { icon = " ", color = "warning", alt = { "WARNING" } },
        PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
        NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
    },
    highlight = {
        before = "fg",
        keyword = "wide",
        after = "fg",
    },
    colors = {
        error = { "LspDiagnosticsDefaultError", "ErrorMsg", "#DC2626" },
        warning = { "LspDiagnosticsDefaultWarning", "WarningMsg", "#FBBF24" },
        info = { "LspDiagnosticsDefaultInformation", "#2563EB" },
        hint = { "LspDiagnosticsDefaultHint", "#10B981" },
        default = { "Identifier", "#7C3AED" },
    },
})

require("tabout").setup({
    tabkey = "",
    backwards_tabkey = "",
    completion = false,
    enable_backwards = true,
    tabouts = {
        { open = "'", close = "'" },
        { open = '"', close = '"' },
        { open = "`", close = "`" },
        { open = "(", close = ")" },
        { open = "[", close = "]" },
        { open = "{", close = "}" },
        { open = "$", close = "$" },
        { open = "$$", close = "$$" },
        { open = "[[", close = "]]" },
        { open = '"""', close = '"""' },
        { open = '"""', close = '"""' },
        { open = "<", close = ">" },
    },
    ignore_beginning = false,
    exclude = {},
})
