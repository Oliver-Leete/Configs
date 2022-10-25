local function lsp_status()
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
            ret = ret .. " " .. table.concat(clients, '   ')
        end
    end
    return ret
end

local function diff_source()
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

local function noice_wrapper()
    local message = require("noice").api.status.message.get()
    return message:sub(1,80)
end

require('lualine').setup({
    options = {
        component_separators = { left = '', right = '' },
        section_separators = { left = '', right = '' },
        globalstatus = true,
        refresh = {
            statusline = 1000,
        }
    },
    sections = {
        lualine_a = { 'mode' },
        lualine_b = {
            { 'b:gitsigns_head', icon = '' },
            { 'diff', source = diff_source, symbols = { added = ' ', modified = ' ', removed = ' ' } },
            { 'diagnostics', symbols = { error = " ", warn = " ", info = " ", hint = " " } },
            { "overseer", symbols = ov_list },
        },
        lualine_c = {
            {
                require("noice").api.status.mode.get,
                cond = require("noice").api.status.mode.has,
                color = { fg = "#ff9e64" },
            },
            {
                noice_wrapper,
                cond = require("noice").api.status.message.has,
            },
            { require("dap").status }
        },
        lualine_x = {},
        lualine_y = { 'encoding', 'filetype', 'fileformat' },
        lualine_z = { lsp_status, 'location' }
    },
})
