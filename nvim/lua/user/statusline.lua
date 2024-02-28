local left = ""
local right = ""
local leftc = ""
local rightc = ""
local leftend = ""
local rightend = ""

require("nvim-navic").setup({
    icons = {
        File = " ",
        Module = " ",
        Namespace = " ",
        Package = " ",
        Class = " ",
        Method = " ",
        Property = " ",
        Field = " ",
        Constructor = " ",
        Enum = " ",
        Interface = " ",
        Function = " ",
        Variable = " ",
        Constant = " ",
        String = " ",
        Number = " ",
        Boolean = " ",
        Array = " ",
        Object = " ",
        Key = " ",
        Null = " ",
        EnumMember = " ",
        Struct = " ",
        Event = " ",
        Operator = " ",
        TypeParameter = " "
    },
})
local type_hl = {
    File = "Directory",
    Module = "@include",
    Namespace = "@namespace",
    Package = "@include",
    Class = "@structure",
    Method = "@method",
    Property = "@property",
    Field = "@field",
    Constructor = "@constructor",
    Enum = "@field",
    Interface = "@type",
    Function = "@function",
    Variable = "@variable",
    Constant = "@constant",
    String = "@string",
    Number = "@number",
    Boolean = "@boolean",
    Array = "@field",
    Object = "@type",
    Key = "@keyword",
    Null = "@comment",
    EnumMember = "@field",
    Struct = "@struct",
    Event = "@keyword",
    Operator = "@operator",
    TypeParameter = "@type",
}
for i, id in pairs(type_hl) do
    local is_hl_set = function(hl_name)
        local exists, hl = pcall(vim.api.nvim_get_hl_by_name, hl_name, true)
        local color = hl.foreground or hl.background or hl.reverse
        return exists and color ~= nil
    end

    local hl
    if is_hl_set(id) then
        hl = vim.api.nvim_get_hl_by_name(id, true)
    else
        hl = vim.api.nvim_get_hl_by_name("Normal", true)
    end
    local name = "NavicIcons" .. i
    hl.background = Ct.diff.change
    vim.api.nvim_set_hl(0, name, hl)
    type_hl[i] = name
end
vim.api.nvim_set_hl(0, "NavicSeparator", { fg = Ct.ui.fg, bg = Ct.syn.fun })
vim.api.nvim_set_hl(0, "NavicText", { fg = Ct.ui.fg, bg = Ct.syn.fun })

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

local st = require("overseer").STATUS
local ov_list = {
    [st.FAILURE] = " ",
    [st.CANCELED] = " ",
    [st.SUCCESS] = " ",
    [st.RUNNING] = " ",
    [st.PENDING] = " ",
}

local recession_wrapper = function()
    local message = require("resession").get_current()
    if message:len() <= 20 then
        return message
    else
        return "..." .. message:sub((message:len() - 17))
    end
end

local noice_wrapper = function()
    local message = require("noice").api.status.message.get()
    return message:sub(1, 80)
end


local is_wide = function()
    return vim.go.columns >= 200
end

local overseer = require("overseer")
OverseerTask = function()
    local bufnr = vim.api.nvim_get_current_buf()
    local task = vim.tbl_filter(function(t) return (t.strategy.bufnr == bufnr) end, overseer.list_tasks())[1]
    return ov_list[task.status] .. task.name
end

local overseericon = function()
    local bufnr = vim.api.nvim_get_current_buf()
    local task = vim.tbl_filter(function(t) return (t.strategy.bufnr == bufnr) end, overseer.list_tasks())[1]
    return ov_list[task.status]
end

local is_overseer = function()
    return vim.bo.buftype == "terminal" and
        0 < #vim.tbl_filter(
            function(t)
                return (vim.api.nvim_get_current_buf() == vim.api.nvim_get_current_buf())
            end,
            overseer.list_tasks()
        )
end

local not_overseer = function()
    return not is_overseer()
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
    tabline = {
        lualine_a = {
            {
                function() return "" end,
                draw_empty = true,
                separator = { left = leftend, right = left },
            },
            {
                "tabs",
                mode = 1,
                use_mode_colors = true,
                show_modified_status = false,
                cond = function()
                    return #vim.api.nvim_list_tabpages() > 1
                end,
                fmt = function(_, context)
                    local ok, tabname = pcall(vim.api.nvim_tabpage_get_var, context.tabId, 'tabname')
                    if ok and tabname and tabname ~= "" then
                        return tabname
                    end
                    return vim.fn.getcwd(-1, context.tabnr):match(".*/(.-)$")
                end,
            },
            {
                function() return "" end,
                draw_empty = true,
                cond = function()
                    return #vim.api.nvim_list_tabpages() > 1
                end,
                separator = "|",
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
                cond = not_overseer,
            },
            {
                OverseerTask,
                cond = is_overseer,
            }
        },
        lualine_b = {
            {
                "navic",
                color_correction = "dynamic",
                navic_opts = { separator = "|", highlight = true, click = true },
                separator = { left = left, right = "" },
                padding = { left = 1, right = 0 },
                cond = not_overseer,
            },
            {
                Filmpicker_endtime,
                separator = { left = left, right = "" },
                padding = { left = 1, right = 0 },
                cond = function() return vim.fn.expand("%") == "/tmp/film_list.films" end,
            },
            {
                function() return " " end,
                cond = not_overseer,
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
                cond = not_overseer,
            },
            {
                Filmpicker_winbar,
                separator = { left = leftend, right = rightend },
                cond = function() return vim.fn.expand("%") == "/tmp/film_list.films" end,
            },
            {
                overseericon,
                separator = { left = right, right = rightend },
                cond = is_overseer,
            },
        },
    },
})
vim.api.nvim_set_hl(0, "StatusLineNC", { fg = Ct.ui.bg_p2, bg = Ct.ui.bg })
