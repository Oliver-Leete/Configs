---@module "lazy"
---@type LazySpec
return {
    {
        "Bekaboo/dropbar.nvim",
        opts = {
            bar = {
                attach_events =
                {
                    "OptionSet",
                    "BufWinEnter",
                    "BufWritePost",
                    "FileType",
                },
                enable = function(buf, win, _)
                    if
                        not vim.api.nvim_buf_is_valid(buf)
                        or not vim.api.nvim_win_is_valid(win)
                        or vim.fn.win_gettype(win) ~= ""
                        or vim.wo[win].winbar ~= ""
                        or vim.bo[buf].ft == "help"
                    then
                        return false
                    end

                    local stat = vim.uv.fs_stat(vim.api.nvim_buf_get_name(buf))
                    if stat and stat.size > 1024 * 1024 then
                        return false
                    end

                    return vim.bo[buf].ft == "markdown"
                        or vim.bo[buf].ft == "oil"
                        or vim.bo[buf].ft == "toggleterm"
                        or pcall(vim.treesitter.get_parser, buf)
                        or not vim.tbl_isempty(vim.lsp.get_clients({
                            bufnr = buf,
                            method = "textDocument/documentSymbol",
                        }))
                end,
            },
            sources = {
                path = {
                    preview = false,
                },
                terminal = {
                    name = function(buf)
                        local name = vim.api.nvim_buf_get_name(buf)
                        local term = require("toggleterm.terminal").find(function(t) return t.bufnr == buf end)
                        if term then return term.display_name or term.name else return name end
                    end
                }
            }
        },
        lazy = false,
        keys = {
            { "<Leader>:", function() require("dropbar.api").pick() end,                desc = "Pick breadcrumb" },
            { "[:",        function() require("dropbar.api").goto_context_start() end,  desc = "Go to start of breadcrumb" },
            { "]:",        function() require("dropbar.api").select_next_context() end, desc = "Select next breadcrumb" },
        },
    },
}
