-- Theme
vim.opt.termguicolors = true

ZenOrFull = function()
    local handle = io.popen([[kitty @ ls | jq ".[].tabs[] | select(.is_focused) | .windows | length"]])
    local num_windows = tonumber(handle:read("*a"))
    handle:close()
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
    commentStyle = "italic",
    functionStyle = "NONE",
    keywordStyle = "NONE",
    statementStyle = "NONE",
    typeStyle = "NONE",
    variablebuiltinStyle = "NONE",
    specialReturn = true,
    specialException = true,
    transparent = false,
    colors = {},
    overrides = {},
    dimInactive = true,
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
    vim.notify(method.message, severity[params.type])
end

require("nvim-gps").setup({
    icons = {
        ["module-name"] = "%#WinBarIcon#%#WinBarText# ",
        ["class-name"] = "%#WinBarIcon#%#WinBarText# ",
        ["function-name"] = "%#WinBarIcon#%#WinBarText# ",
        ["method-name"] = "%#WinBarIcon#%#WinBarText# ",
        ["macro-name"] = "%#WinBarIcon#亮%#WinBarText#",
        ["container-name"] = "%#WinBarAltIcon#%#WinBarText# ",
        ["tag-name"] = "%#WinBarIcon#炙%#WinBarText#",
    },
    languages = {
        ["latex"] = {
            icons = {
                ["title-name"] = "%#WinBarIcon## %#WinBarText#",
                ["label-name"] = "%#WinBarIcon# %#WinBarText# ",
            },
        },
    },
})

vim.api.nvim_set_hl(0, "WinBarSigActParm", { fg = "#7E9CD8", bg = "#262626" })

vim.api.nvim_set_hl(0, "WinBar", { fg = "#727169", bg = "#262626", bold = true })
vim.api.nvim_set_hl(0, "WinBarNC", { fg = "#727169", bg = "#262626", bold = true })

vim.api.nvim_set_hl(0, "WinBarText", { fg = "#727169", bg = "#262626", bold = true })
vim.api.nvim_set_hl(0, "WinBarIcon", { fg = "#957FB8", bg = "#262626", bold = false })
vim.api.nvim_set_hl(0, "WinBarAltIcon", { fg = "#7E9CD8", bg = "#262626", bold = false })
function GPS_Bar()
    local winbar = ""
    local columns = vim.api.nvim_get_option("columns")
    local sig = require("lsp_signature").status_line(columns)

    if vim.fn.expand("%") ~= "" then
        local icon = require("nvim-web-devicons").get_icon(vim.fn.expand("%:t"), vim.fn.expand("%:e"))
        if icon == "" or icon == nil then
            icon = ""
        end
        winbar = winbar .. "%#WinBarIcon#" .. icon .. "%#WinBarText# " .. "%f"
    end
    if require("nvim-gps").is_available() then
        local location = require("nvim-gps").get_location()
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
    if winbar ~= "" then
        winbar = "%=" .. winbar .. "%="
    end
    return winbar
end

vim.go.winbar = "%{%v:lua.GPS_Bar()%}"
