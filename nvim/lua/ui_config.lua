-- Theme
vim.opt.termguicolors = true

ZenOrFull = function()
    local handle = io.popen([[kitty @ ls | jq ".[].tabs[] | select(.is_focused) | .windows | length"]])
    local num_windows
    if handle then
        num_windows = tonumber(handle:read("*a"))
        handle:close()
    else
        return
    end

    if num_windows > 1 then
        vim.cmd([[silent !xdotool key --clearmodifiers "ctrl+alt+f"]])
    else
        require("zen-mode").toggle({})
    end
end
vim.api.nvim_create_user_command("ZenOrFull", ZenOrFull, { nargs = 0 })

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

require("dressing").setup({
    select = {
        backend = { "telescope" },
        telescope = require("telescope.themes").get_ivy({
            height = 30,
        }),
    },
})

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

vim.api.nvim_set_hl(0, "WinBarSigActParm", { fg = "#7E9CD8" })


vim.api.nvim_set_hl(0, "WinBarActive", { fg = "#FF9E3B", bg = "#1F1F28", bold = true })
vim.api.nvim_set_hl(0, "WinBar", { fg = "#727169", bg = "#1F1F28", bold = true })
vim.api.nvim_set_hl(0, "WinBarNC", { fg = "#727169", bg = "#1F1F28", bold = true })
vim.api.nvim_set_hl(0, "WinBarIcon", { fg = "#957FB8", bg = "#1F1F28", bold = false })
vim.api.nvim_set_hl(0, "WinBarIconNC", { fg = "#957FB8", bg = "#1F1F28", bold = true })

vim.api.nvim_set_hl(0, "WinBarAltIcon", { fg = "#7E9CD8", bold = false })

require("nvim-navic").setup({ highlight = true })
vim.api.nvim_set_hl(0, "NavicText", { link = "WinBar" })
vim.api.nvim_set_hl(0, "NavicSeparator", { link = "WinBar" })

function GPS_Bar()
    local is_active = vim.api.nvim_get_current_win() == tonumber(vim.g.actual_curwin)
    local winbar = ""
    local hld = "%#WinBar#"
    local hl = "%#WinBar#"
    local hli = "%#WinBarIcon#"
    if is_active then
        hl = "%#WinBarActive#"
    end

    -- Special winbar for filmpicker script
    if vim.bo[vim.api.nvim_get_current_buf()].filetype == "filmlist" then
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
        winbar = winbar .. hl .. string.format("%02d:%02d:%02d", hours, minutes, seconds) .. hld

    -- Special winbar for terminals
    elseif vim.bo[vim.api.nvim_get_current_buf()].filetype == "toggleterm" then
        local term_name
        if vim.b[0].my_term_title then
            term_name = vim.b[0].my_term_title
        else
            term_name = "Terminal " .. tostring(vim.b[0].toggle_number)
        end
        winbar = winbar .. hl .. term_name .. hld

    -- Default winbar
    else
        local columns = vim.api.nvim_get_option("columns")
        local sig = require("lsp_signature").status_line(columns)

        if vim.fn.expand("%") ~= "" then
            local icon = require("nvim-web-devicons").get_icon(vim.fn.expand("%:t"), vim.fn.expand("%:e"))
            if icon == "" or icon == nil then
                icon = ""
            end
            winbar = winbar .. hli .. icon .. hl .. " %f" .. hld
        end
        if is_active then
            if require("nvim-navic").is_available() then
                local location = require("nvim-navic").get_location()
                if location ~= "" then
                    winbar = winbar .. " > " .. location
                end
            end
            if sig.label ~= nil and sig.label ~= "" and vim.api.nvim_get_mode().mode == "i" then
                local label1 = sig.label
                local label2 = ""
                local range = {}
                if sig.range and (sig.range["start"] ~= 0 and sig.range["end"] ~= 0) then
                    range[1] = sig.range["start"]
                    range[2] = sig.range["end"]
                elseif sig.hint ~= "" then
                    range[1], range[2] = label1:find(sig.hint)
                end
                if range[1] and range[2] then
                    label1 = sig.label:sub(1, sig.range["start"] - 1)
                    label2 = sig.label:sub(sig.range["end"] + 1, #sig.label)
                end
                winbar = winbar
                    .. " > "
                    .. "%#WinBarText#"
                    .. label1
                    .. "%*"
                    .. "%#WinBarSigActParm#"
                    .. sig.hint
                    .. "%*"
                    .. "%#WinBarText#"
                    .. label2
            end
        end
    end
    if winbar ~= "" then
        winbar = "%=" .. winbar .. "%="
    end
    return winbar
end

vim.go.winbar = "%{%v:lua.GPS_Bar()%}"

local filetypes = vim.api.nvim_create_augroup("winbars ", { clear = true })
vim.api.nvim_create_autocmd("BufEnter",
    { pattern = "*.films", callback = function() vim.b[0].filetype = "filmlist" end, group = filetypes })
