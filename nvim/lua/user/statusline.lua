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
            ret = ret .. " " .. table.concat(clients, "   ")
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

local harp = require("harpoon.mark")
local harpoon = function()
    return "[⥣]"
end

local harpoon_cond = function()
    local bufname = vim.api.nvim_buf_get_name(0)
    return harp.valid_index(harp.get_index_of(bufname))
end

local is_wide = function()
    return vim.go.columns >= 200
end

require("lualine").setup({
    options = {
        component_separators = { left = "", right = "" },
        section_separators = { left = "", right = "" },
        globalstatus = true,
        refresh = {
            statusline = 1000,
        }
    },
    sections = {
        lualine_a = {
            "mode",
            {
                require("hydra.statusline").get_name,
                cond = require("hydra.statusline").is_active,
            },
            {
                harpoon,
                cond = harpoon_cond,
                on_click = function() vim.defer_fn(require("harpoon.ui").toggle_quick_menu, 100) end,
            },
        },
        lualine_b = {
            {
                "b:gitsigns_head",
                icon = "",
                on_click = function() vim.defer_fn(function() vim.cmd("DiffviewOpen") end, 100) end,
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
                on_click = function() vim.defer_fn(function() vim.cmd("Telescope diagnostics bufnr=0 theme=get_ivy") end
                        , 100)
                end,
            },
            {
                "overseer",
                symbols = ov_list,
                on_click = function() vim.defer_fn(require("overseer").toggle, 100) end,
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
                on_click = function() vim.defer_fn(function() vim.cmd("Telescope current_buffer_fuzzy_find") end, 100 ) end

            },
        }
    },
})
