-- Theme
vim.opt.termguicolors = true

require("stickybuf").setup({
    filetype = {
        OverseerList = "filetype",
    },
    buftype = {
        help     = "buftype",
        terminal = "buftype",
    },
})

require("zen-mode").setup({
    window = {
        options = {
            signcolumn = "no",
            number = false,
            relativenumber = false,
        },
    },
    plugins = {
        options = {
            enabled = true,
            ruler = false,
            showcmd = false,
        },
    },
    on_open = function()
        vim.diagnostic.disable()
    end,
    on_close = function()
        vim.cmd([[silent lua vim.diagnostic.enable()]])
    end,
})

require("dressing").setup({
    select = {
        backend = { "telescope" },
        telescope = require("telescope.themes").get_ivy({
            height = 30,
        }),
    },
})

require("kanagawa").setup({
    undercurl = true,
    commentStyle = { italic = true },
    functionStyle = {},
    keywordStyle = {},
    statementStyle = {},
    typeStyle = {},
    variablebuiltinStyle = {},
    specialReturn = true,
    specialException = true,
    transparent = false,
    colors = {},
    overrides = {},
    dimInactive = false,
    globalStatus = true,
})

vim.cmd("colorscheme kanagawa")
local tc = require("kanagawa.colors").setup()

local background = vim.api.nvim_get_hl_by_name("CursorLine", true).background

local sign_colours = { Add = "Added", Change = "Changed", Delete = "Deleted" }
for sign, colour in pairs(sign_colours) do
    local highlight = vim.api.nvim_get_hl_by_name("diff" .. colour, true)
    highlight.background = background
    vim.api.nvim_set_hl(0, "GitSigns" .. sign .. "Cul", highlight)
end

vim.api.nvim_set_hl(0, "CursorLineNr", { fg = tc.roninYellow, bg = tc.sumiInk3, bold = true })
vim.api.nvim_set_hl(0, "CursorLineSign", { link = "CursorLine" })
vim.api.nvim_set_hl(0, "CursorLineFold", { link = "CursorLine" })

vim.g.matchup_matchparen_deferred = true
vim.g.matchup_matchparen_hi_surround_always = true

vim.api.nvim_set_hl(0, "IndentBlanklineContextChar", { link = "MatchParen" })
vim.api.nvim_set_hl(0, "IndentBlanklineContextStart", { underline = true, sp = "#ff9e3b" })


require("indent_blankline").setup {
    char = "",
    context_char = "🭰",
    space_char_blankline = " ",
    show_current_context = true,
    show_current_context_start = true,
    context_patterns = {
        "class",
        "^func",
        "method",
        "^if",
        "while",
        "for",
        "with",
        "try",
        "except",
        "arguments",
        "argument_list",
        "object",
        "dictionary",
        "element",
        "table",
        "tuple",
        "do_block",
        "array",
        "struct",
    },
}

local stages_util = require("notify.stages.util")
require("notify").setup({
    stages = {
        function(state)
            local next_height = state.message.height + 2
            local next_row = stages_util.available_slot(
                state.open_windows,
                next_height,
                stages_util.DIRECTION.BOTTOM_UP
            )
            if not next_row then
                return nil
            end
            return {
                relative = "editor",
                anchor = "NE",
                width = state.message.width,
                height = state.message.height,
                col = vim.opt.columns:get(),
                row = next_row,
                border = "rounded",
                style = "minimal",
                opacity = 0,
            }
        end,
        function()
            return {
                opacity = { 100 },
                col = { vim.opt.columns:get() },
            }
        end,
        function()
            return {
                col = { vim.opt.columns:get() },
                time = true,
            }
        end,
        function()
            return {
                width = {
                    1,
                    frequency = 2.5,
                    damping = 0.9,
                    complete = function(cur_width)
                        return cur_width < 3
                    end,
                },
                opacity = {
                    0,
                    frequency = 2,
                    complete = function(cur_opacity)
                        return cur_opacity <= 4
                    end,
                },
                col = { vim.opt.columns:get() },
            }
        end,
    },
})

vim.notify = require("notify")

-- LSP integration
local severity = {
    "error",
    "warn",
    "info",
    "info", -- map both hint and info to info?
}
vim.lsp.handlers["window/showMessage"] = function(_, method, params, _)
    ---@diagnostic disable-next-line: redundant-parameter
    vim.notify(method.message, severity[params.type])
end

local mode_colours = { Normal = tc.crystalBlue, Insert = tc.autumnGreen, Visual = tc.oniViolet, Replace = tc.autumnRed,
    Command = tc.boatYellow2, Inactive = tc.fujiGray }
for mode, colour in pairs(mode_colours) do
    vim.api.nvim_set_hl(0, "WinBar" .. mode, { fg = tc.bg, bg = colour, bold = true })
    vim.api.nvim_set_hl(0, "WinBar" .. mode .. "Ends", { fg = colour, bg = tc.bg, bold = true })
end
vim.api.nvim_set_hl(0, "WinBarInactiveSpecial", { fg = tc.bg, bg = tc.waveBlue2, bold = true })
vim.api.nvim_set_hl(0, "WinBarInactiveSpecialEnds", { fg = tc.waveBlue2, bg = tc.bg, bold = true })
vim.api.nvim_set_hl(0, "WinBarBlank", { fg = tc.sumiInk, bg = tc.sumiInk })
vim.api.nvim_set_hl(0, "WinBarBlank", { fg = tc.sumiInk, bg = tc.sumiInk })

require("nvim-navic").setup({ highlight = false })

local function filmpicker_winbar(hl)
    local line1 = vim.fn.search([[\(\%^\|^$\)]], "nbWc") - 1
    local line2 = vim.fn.search([[\(\%$\|^$\)]], "nW")

    local lines = vim.api.nvim_buf_get_lines(0, line1, line2, false)
    local pattern = "(%d+):(%d+):(%d+)"
    local runtime = 0
    for _, line in pairs(lines) do
        local time_string = line:sub(1, 8)
        local hour, minute, second = time_string:match(pattern)
        if hour and minute and second then
            runtime = runtime + hour * 3600 + minute * 60 + second
        end
    end
    local hours = math.floor(runtime / 3600)
    local minutes = math.floor(math.fmod(runtime, 3600) / 60)
    local seconds = math.floor(math.fmod(runtime, 60))
    return hl .. string.format("%02d:%02d:%02d", hours, minutes, seconds)
end

SpecialName = function(bufnr)
    if vim.bo[bufnr].filetype == "help" then
        local help_title = vim.fn.expand("%:t:r")
        help_title = help_title:gsub("(%l)(%w*)", function(a, b) return string.upper(a) .. b end)
        return help_title .. " Help"
    else
        local filetype = Special_types[vim.bo[bufnr].filetype].name
        if filetype then
            return filetype
        end
    end
    return false
end

local function special_winbar(bufnr, hl, is_active)
    local mode = _G.WindLine.state.mode[2]
    local specialname = SpecialName(bufnr)
    if not specialname then return "" end
    if is_active then
        hl = "%#WinBar" .. mode .. "#"
    else
        hl = "%#WinBarInactiveSpecial#"
    end
    return hl .. specialname
end

local function default_winbar(bufnr, hl, is_active)
    local winbar = hl
    -- Ismodified icon
    if vim.bo.modified or vim.bo.modifiable == false then
        winbar = winbar .. " "
    end
    -- File icon
    if vim.fn.expand("%") ~= "" then
        local icon = require("nvim-web-devicons").get_icon(vim.fn.expand("%:t"), vim.fn.expand("%:e"))
        if icon == "" or icon == nil then
            icon = ""
        end
        winbar = winbar .. icon .. " "
        winbar = winbar .. require("windline.utils").get_unique_bufname(bufnr)
    end
    -- Code location
    if is_active and require("nvim-navic").is_available() then
        local location = require("nvim-navic").get_location()
        if location ~= "" then
            winbar = winbar .. " > " .. location
        end
    end
    return winbar
end

function _G.my_toggleterm_winbar_click(id)
    local cur_win = math.floor(id / 1000)
    print(cur_win)
    local term_id = id - cur_win * 1000
    print(term_id)
    local term = require("toggleterm.terminal").get_or_create_term(term_id)
    if not term then return end
    vim.api.nvim_win_set_buf(cur_win, term.bufnr)
end

function GPS_Bar()
    local bufnr = vim.api.nvim_get_current_buf()
    local is_active = vim.api.nvim_get_current_win() == tonumber(vim.g.actual_curwin)

    local mode = _G.WindLine.state.mode[2]
    if not mode then return "" end

    local hlb = "%#WinBarBlank#"
    local hl = is_active and "%#WinBar" .. mode .. "#" or "%#WinBarInactive#"
    local hle = is_active and "%#WinBar" .. mode .. "Ends#" or "%#WinBarInactiveEnds#"

    local winbar = ""
    -- Special winbar for filmpicker script
    if vim.api.nvim_buf_get_name(bufnr) == "/tmp/film_list.films" then
        winbar = filmpicker_winbar(hl)
    elseif vim.bo[bufnr].filetype == "toggleterm" then
        local term_name = vim.b[0].my_term_title
        local term_list = require("toggleterm.terminal").get_all(true)
        local cur_win = vim.api.nvim_get_current_win()
        for _, term in pairs(term_list) do
            local is_cur_term = term_name == term.jobname
            if is_active and is_cur_term then
                hl = "%#WinBar" .. mode .. "#"
                hle = "%#WinBar" .. mode .. "Ends#"
            elseif is_cur_term then
                hl = "%#WinBarInactiveSpecial#"
                hle = "%#WinBarInactiveSpecialEnds#"
            else
                hl = "%#WinBarInactive#"
                hle = "%#WinBarInactiveEnds#"
            end
            winbar = winbar ..
                hle ..
                string.format("%%%d@v:lua.my_toggleterm_winbar_click@", term.id + cur_win * 1000) ..
                "" .. hl .. " " .. term.jobname .. hle .. " "
        end
        return hlb .. winbar .. hlb .. "%="
    elseif Is_special(bufnr) then
        winbar = special_winbar(bufnr, hl, is_active)
        hle = is_active and "%#WinBar" .. mode .. "Ends#" or "%#WinBarInactiveSpecialEnds#"
    else -- Default winbar
        winbar = default_winbar(bufnr, hl, is_active)
    end

    if winbar == "" then return "" end
    return hlb .. hle .. "" .. winbar .. hle .. "" .. hlb .. "%=" .. hlb
end

vim.go.winbar = "%{%v:lua.GPS_Bar()%}"
