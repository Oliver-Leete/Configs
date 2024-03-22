local left = ""
local right = ""
local leftc = ""
local rightc = ""
local leftend = ""
local rightend = ""

local lsp_status = function()
    local ret = ""
    local bufnr = vim.api.nvim_get_current_buf()
    if vim.lsp.get_active_clients({ bufnr = bufnr }) then
        local clients = {}
        for _, client in pairs(vim.lsp.get_active_clients({ bufnr = bufnr or vim.api.nvim_get_current_buf() })) do
            if client.name ~= "null-ls" then
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

local noice_wrapper = function()
    local message = require("noice").api.status.message.get()
    return message:sub(1, 80)
end


local is_wide = function()
    return vim.go.columns >= 200
end

require("lualine").setup({
    options = {
        component_separators = { left = leftc, right = rightc },
        section_separators = { left = left, right = right },
        globalstatus = true,
        theme = "my_kanagawa",
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
                separator = { left = leftend, right = left },
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
                "b:gitsigns_head",
                icon = "",
                on_click = function() vim.defer_fn(function() vim.cmd("Telescope git_branches") end, 100) end,
                separator = { left = "", right = "" },
            },
            {
                function() return "" end,
                draw_empty = true,
                separator = { left = "", right = rightend },
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
                separator = { left = leftend, right = "" },
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
                separator = { left = right, right = rightend },
            },
        }
    },
    winbar = {
        lualine_a = {
            {
                function() return "" end,
                draw_empty = true,
                separator = { left = leftend, right = left },
            },
            {
                "filename",
                path = 1,
                symbols = {
                    modified = "󰷫 ",
                    readonly = "󰏯 ",
                    unnamed = "",
                    newfile = "󰎔 ",
                },
            },
        },
        lualine_b = {
            {
                Filmpicker_endtime,
                separator = { left = left, right = "" },
                padding = { left = 1, right = 0 },
                cond = function() return vim.fn.expand("%") == "/tmp/film_list.films" end,
            },
            {
                function() return " " end,
                draw_empty = true,
                padding = { left = 0, right = 0 }
            },
            {
                function() return "" end,
                draw_empty = true,
                separator = { left = left, right = rightend },
            },
        },
        lualine_c = {},
        lualine_y = {
            {
                function() return "" end,
                draw_empty = true,
                separator = { left = leftend, right = "" },
                padding = { left = 0, right = 0 },
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
                separator = { left = right, right = rightend },
            },
            {
                Filmpicker_winbar,
                separator = { left = leftend, right = rightend },
                cond = function() return vim.fn.expand("%") == "/tmp/film_list.films" end,
            },
        },
    },
    inactive_winbar = {
        lualine_a = {
            {
                function() return "" end,
                draw_empty = true,
                separator = { left = leftend, right = left },
            },
            {
                "filename",
                path = 1,
                symbols = {
                    modified = "󰷫 ",
                    readonly = "󰏯 ",
                    unnamed = "",
                    newfile = "󰎔 ",
                },
            },
        },
        lualine_b = {
            {
                Filmpicker_endtime,
                separator = { left = left, right = "" },
                padding = { left = 1, right = 0 },
                cond = function() return vim.fn.expand("%") == "/tmp/film_list.films" end,
            },
            {
                function() return " " end,
                draw_empty = true,
                padding = { left = 0, right = 0 }
            },
            {
                function() return "" end,
                draw_empty = true,
                separator = { left = left, right = rightend },
            },
        },
        lualine_c = {},
        lualine_y = {
            {
                function() return "" end,
                draw_empty = true,
                separator = { left = leftend, right = "" },
                padding = { left = 0, right = 0 },
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
                separator = { left = right, right = rightend },
            },
            {
                Filmpicker_winbar,
                separator = { left = leftend, right = rightend },
                cond = function() return vim.fn.expand("%") == "/tmp/film_list.films" end,
            },
        },
    },
})
vim.api.nvim_set_hl(0, "StatusLineNC", { fg = Ct.ui.bg_p2, bg = Ct.ui.bg })
