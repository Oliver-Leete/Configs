-- Theme
vim.opt.termguicolors = true

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
    separator = " %#WinBarSep#>%#WinBarText# ",
})

vim.api.nvim_set_hl(0, "WinBar", { fg = "#727169", bg = "#181820", bold = true })
vim.api.nvim_set_hl(0, "WinBarNC", { fg = "#727169", bg = "#181820", bold = true })

vim.api.nvim_set_hl(0, "WinBarText", { fg = "#727169", bg = "#181820", bold = true })
vim.api.nvim_set_hl(0, "WinBarIcon", { fg = "#957FB8", bg = "#181820", bold = false })
vim.api.nvim_set_hl(0, "WinBarAltIcon", { fg = "#7E9CD8", bg = "#181820", bold = false })
vim.api.nvim_set_hl(0, "WinBarSep", { fg = "#DCD7BA", bg = "#181820", bold = true })

function GPS_Bar()
    local winbar = ""
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
            winbar = winbar .. " %#WinBarSep#>%#WinBarText# " .. location
        end
    end
    if winbar ~= "" then
        winbar = "%=" .. winbar .. "%="
    end
    return winbar
end

vim.wo.winbar = "%{%v:lua.GPS_Bar()%}"
