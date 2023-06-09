local left = ""
local right = ""
local leftc = ""
local rightc = ""

local lsp_status = function()
    local ret = ""
    local bufnr = vim.api.nvim_get_current_buf()
    if vim.lsp.get_active_clients({ bufnr = bufnr }) then
        local clients = {}
        for _, client in pairs(vim.lsp.get_active_clients({ bufnr = bufnr or vim.api.nvim_get_current_buf() })) do
            if client.name == "jedi_language_server" then
                clients[#clients + 1] = "jls"
            elseif client.name ~= "null-ls" then
                clients[#clients + 1] = client.name
            end
        end
        if next(clients) then
            ret = ret .. table.concat(clients, " " .. rightc .. " ")
        end
    end
    return ret
end

local diff_source = function()
    local gitsigns = vim.b.gitsigns_status_dict
    if gitsigns then
        return {
            added = gitsigns.added,
            modified = gitsigns.changed,
            removed = gitsigns.removed
        }
    end
end

local st = require("overseer").STATUS
local ov_list = {
    [st.FAILURE] = " ",
    [st.CANCELED] = " ",
    [st.SUCCESS] = " ",
    [st.RUNNING] = " ",
    [st.PENDING] = " ",
}

local noice_wrapper = function()
    local message = require("noice").api.status.message.get()
    return message:sub(1, 80)
end

local recession_wrapper = function()
    local message = require("resession").get_current()
    if message:len() <= 20 then
        return message
    else
        return "..." .. message:sub((message:len() - 17))
    end
end

local grapple = function()
    return "󱡅 "
end

local is_wide = function()
    return vim.go.columns >= 200
end

require("lualine").setup({
    options = {
        component_separators = { left = leftc, right = rightc },
        section_separators = { left = left, right = right },
        globalstatus = true,
        refresh = {
            statusline = 1000,
        },
        disabled_filetypes = {
            statusline = {},
            winbar = vim.tbl_keys(require("user.myfuncs").special_types),
        },
    },
    sections = {
        lualine_a = {
            {
                function() return "" end,
                draw_empty = true,
                separator = { left = " ", right = left },
            },
            {
                "mode",
            },
            {
                require("hydra.statusline").get_name,
                cond = require("hydra.statusline").is_active,
            },
        },
        lualine_b = {
            {
                "overseer",
                symbols = ov_list,
                on_click = function() vim.defer_fn(require("overseer").toggle, 100) end,
            },
            {
                recession_wrapper,
                cond = function() return require("resession").get_current() ~= nil end,
                on_click = function() vim.defer_fn(function() require("resession").load() end, 100) end

            },
            {
                "b:gitsigns_head",
                icon = "",
                on_click = function() vim.defer_fn(function() vim.cmd("Telescope git_branches") end, 100) end,
                separator = { left = "", right = "" },
            },
            {
                function() return "" end,
                draw_empty = true,
                separator = { left = "", right = "" },
            },
        },
        lualine_c = {
            {
                noice_wrapper,
                cond = require("noice").api.status.message.has,
                on_click = function() vim.defer_fn(function() vim.cmd("Noice") end, 100) end,
            },
            {
                require("dap").status,
                on_click = function() vim.defer_fn(require("dap").continue, 100) end,
            }
        },
        lualine_x = {
            {
                require("noice").api.status.mode.get,
                cond = require("noice").api.status.mode.has,
                color = { fg = "#ff9e64" },
            },
        },
        lualine_y = {
            {
                function() return "" end,
                draw_empty = true,
                separator = { left = "", right = "" },
            },
            {
                "encoding",
                cond = function() return vim.bo.fenc ~= "utf-8" and vim.go.enc ~= "utf-8" end,
            },
            {
                "filetype",
                cond = is_wide,
            },
            {
                "fileformat",
                cond = function() return vim.bo.fileformat ~= "unix" end,
            }
        },
        lualine_z = {
            {
                lsp_status,
                on_click = function() vim.defer_fn(function() vim.cmd("LspInfo") end, 100) end,
            },
            {
                "location",
            },
            {
                function() return "" end,
                draw_empty = true,
                separator = { left = right, right = " " },
            },
        }
    },
    winbar = {
        lualine_a = {
            {
                function() return "" end,
                draw_empty = true,
                separator = { left = " ", right = left },
            },
            {
                grapple,
                cond = require("grapple").exists,
                on_click = function() vim.defer_fn(require("grapple").popup_tags, 100) end,
            },
            {
                "filename",
                path = 1,
                symbols = {
                    modified = "󰷫 ", -- Text to show when the file is modified.
                    readonly = "󰏯 ", -- Text to show when the file is non-modifiable or readonly.
                    unnamed = "",    -- Text to show for unnamed buffers.
                    newfile = "󰎔 ", -- Text to show for newly created file before first write
                },
            },
        },
        lualine_b = {
            {
                "navic",
                color_correction = "dynamic",
                navic_opts = { separator = "  ", highlight = true, },
                separator = { left = left, right = "" },
                padding = { left = 1, right = 0 }
            },
            {
                function() return " " end,
                draw_empty = true,
                separator = { left = left, right = "" },
                padding = { left = 0, right = 0 }
            },
        },
        lualine_c = {},
        lualine_y = {
            {
                function() return "" end,
                draw_empty = true,
                separator = { left = "", right = "" },
                padding = { left = 0, right = 0 }
            },
            {
                "diff",
                source = diff_source,
                symbols = { added = " ", modified = " ", removed = " " },
                on_click = function() vim.defer_fn(function() vim.cmd("DiffviewOpen") end, 100) end,
            },
            {
                "diagnostics",
                symbols = { error = " ", warn = " ", info = " ", hint = "󰅽 " },
                on_click = function()
                    vim.defer_fn(function() vim.cmd("Telescope diagnostics bufnr=0 theme=get_ivy") end,
                        100)
                end,
            },
        },
        lualine_z = {
            {
                "filetype",
                colored = false,
                icon_only = true,
                separator = { left = right, right = " " },
            },
        },
    },
    inactive_winbar = {
        lualine_a = {
            {
                function() return "" end,
                draw_empty = true,
                separator = { left = " ", right = left },
            },
            {
                grapple,
                cond = require("grapple").exists,
                on_click = function() vim.defer_fn(require("grapple").popup_tags, 100) end,
            },
            {
                "filename",
                path = 1,
                symbols = {
                    modified = "󰷫 ", -- Text to show when the file is modified.
                    readonly = "󰏯 ", -- Text to show when the file is non-modifiable or readonly.
                    unnamed = "",    -- Text to show for unnamed buffers.
                    newfile = "󰎔 ", -- Text to show for newly created file before first write
                },
                separator = { left = " ", right = "" },
            },
        },
        lualine_x = {},
        lualine_y = {},
        lualine_z = {
            {
                "filetype",
                colored = false,
                icon_only = true,
                separator = { left = "", right = " " },
            },
        },
    },
})
vim.api.nvim_set_hl(0, "StatusLineNC", { fg = Tc.sumiInk5, bg = Ct.ui.bg })
