local line_setup = function()
    require("incline").setup {
        render = function(props)
            local filename = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(props.buf), ":t")
            if filename == "" then
                filename = "[No Name]"
            end
            local ft_icon, ft_color = require("nvim-web-devicons").get_icon_color(filename)

            local function get_diagnostic_label()
                local icons = { error = " ", warn = " ", info = " ", hint = "󰅽 " }
                local label = {}

                for severity, icon in pairs(icons) do
                    local n = #vim.diagnostic.get(props.buf,
                        { severity = vim.diagnostic.severity[string.upper(severity)] })
                    if n > 0 then
                        table.insert(label, { icon .. n .. " ", group = "DiagnosticSign" .. severity })
                    end
                end
                if #label > 0 then
                    table.insert(label, { "┊ " })
                end
                return label
            end

            return {
                { get_diagnostic_label() },
                { (ft_icon or "") .. " ", guifg = ft_color,                                            guibg = "none" },
                { filename .. " ",        gui = vim.bo[props.buf].modified and "bold,italic" or "bold" },
            }
        end,
    }

    local left = ""
    local right = ""
    local leftc = ""
    local rightc = ""
    local leftend = ""
    local rightend = ""

    local lsp_status = function()
        local ret = ""
        local bufnr = vim.api.nvim_get_current_buf()
        local ac_clients = vim.lsp.get_clients({ bufnr = bufnr })
        if ac_clients then
            local clients = {}
            for _, client in pairs(ac_clients) do
                if client.name ~= "null-ls" then
                    clients[#clients + 1] = client.name
                end
            end
            if next(clients) then
                ret = ret .. table.concat(clients, " | ")
            end
        end
        return ret
    end

    local mini_diff = function()
        local summary = vim.b.minidiff_summary
        if summary then
            return {
                added = summary.add,
                modified = summary.change,
                removed = summary.delete
            }
        end
    end

    local mini_git = function()
        local summary = vim.b.minigit_summary
        if not summary.head_name then return "" end
        return " " .. summary.head_name
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
            disabled_filetypes = { statusline = {}, },
        },
        sections = {
            lualine_a = { { "mode", }, },
            lualine_b = {
                { mini_git, separator = { left = "", right = "" }, },
                { "diff", source = mini_diff, symbols = { added = " ", modified = " ", removed = " " }, },
                { Filmpicker_endtime, cond = function() return vim.fn.expand("%") == "/tmp/film_list.films" end, },
                { Filmpicker_winbar, cond = function() return vim.fn.expand("%") == "/tmp/film_list.films" end, },
            },
            lualine_c = {
                { require("dap").status, },
            },
            lualine_x = {},
            lualine_y = {
                { "encoding",   cond = function() return vim.bo.fenc ~= "utf-8" and vim.go.enc ~= "utf-8" end, },
                { "filetype", },
                { "fileformat", cond = function() return vim.bo.fileformat ~= "unix" end, }
            },
            lualine_z = {
                { lsp_status, },
                { "location", },
            }
        },
    })
    vim.api.nvim_set_hl(0, "StatusLineNC", { fg = Ct.ui.bg_p2, bg = Ct.ui.bg })
end
return {
    "nvim-lualine/lualine.nvim",
    dependencies = {
        { "kyazdani42/nvim-web-devicons" },
        { "b0o/incline.nvim" },
    },
    config = line_setup
}
