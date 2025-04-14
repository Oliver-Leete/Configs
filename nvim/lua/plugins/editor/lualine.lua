---@module "lazy"
---@type LazySpec
return {
    "nvim-lualine/lualine.nvim",
    opts =
    {
        options = {
            component_separators = { left = "", right = "" },
            section_separators = { left = "", right = "" },
            globalstatus = true,
            theme = "tokyonight",
            refresh = {
                statusline = 1000,
            },
        },
        sections = {
            lualine_a = {
                {
                    "mode",
                },
            },
            lualine_b = {
                {
                    function()
                        local summary = vim.b.minigit_summary
                        if not summary.head_name then return "" end
                        return " " .. summary.head_name
                    end,
                },
                {
                    "diff",
                    source = function()
                        local summary = vim.b.minidiff_summary
                        if summary then
                            return {
                                added = summary.add,
                                modified = summary.change,
                                removed = summary.delete
                            }
                        end
                    end,
                    symbols = { added = " ", modified = " ", removed = " " },
                },
                {
                    "diagnostics",
                    symbols = { error = " ", warn = " ", info = " ", hint = "󰅽 " },
                },
                {
                    function() return require("user.filmpicker").endtime() end,
                    cond = function() return vim.fn.expand("%") == "/tmp/film_list.films" end,
                },
                {
                    function() return require("user.filmpicker").runtime() end,
                    cond = function() return vim.fn.expand("%") == "/tmp/film_list.films" end,
                },
            },
            lualine_c = {
                {
                    function() if package.loaded.dap then return require("dap").status() else return "" end end,
                },
            },
            lualine_x = {
            },
            lualine_y = {
                {
                    "encoding",
                    cond = function() return vim.bo.fenc ~= "utf-8" and vim.go.enc ~= "utf-8" end,
                },
                {
                    "filetype",
                },
                {
                    "fileformat",
                    cond = function() return vim.bo.fileformat ~= "unix" end,
                }
            },
            lualine_z = {
                {
                    function()
                        local ret = ""
                        local bufnr = vim.api.nvim_get_current_buf()
                        local ac_clients = vim.lsp.get_clients({ bufnr = bufnr })
                        if ac_clients then
                            local clients = vim.tbl_map(function(client) return client.name end, ac_clients)
                            if next(clients) then ret = ret .. table.concat(clients, " | ") end
                        end
                        return ret
                    end,
                },
                {
                    "location",
                },
            }
        },
    }
}
