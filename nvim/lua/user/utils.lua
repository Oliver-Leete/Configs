local M = {}

M.get_pos_lang = function()
    local c = vim.api.nvim_win_get_cursor(0)
    local range = { c[1] - 1, c[2], c[1] - 1, c[2] }
    local buf = vim.api.nvim_get_current_buf()
    local ok, parser = pcall(
        vim.treesitter.get_parser,
        buf,
        vim.treesitter.language.get_lang(vim.bo[buf].ft)
    )
    if not ok or not parser then
        return ""
    end
    local current_tree = parser:language_for_range(range)
    return current_tree:lang()
end

return M
