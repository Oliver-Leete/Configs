local M = {}

---@return string # The filetype of the node at the cursor, or of the buffer if there is no treesitter parser
M.get_pos_lang = function()
    local cursor = vim.api.nvim_win_get_cursor(0)
    local range = { cursor[1] - 1, cursor[2], cursor[1] - 1, cursor[2] }
    local buf = vim.api.nvim_get_current_buf()
    local ok, parser = pcall(vim.treesitter.get_parser, buf, vim.treesitter.language.get_lang(vim.bo[buf].ft))
    if not ok or not parser then return vim.bo[buf].filetype end
    local current_tree = parser:language_for_range(range)
    return current_tree:lang()
end

---Reduce a list like table
---@generic a
---@generic A table<string, a>
---@param func function(left: a, right: a): a
---@param list A[]
---@param init A
---@return A
M.tbl_lreduce = function(func, list, init)
    local ret = vim.deepcopy(init)
    for _, item in ipairs(list) do
        for k, v in pairs(item) do
            ret[k] = func(ret[k], v)
        end
    end
    return ret
end

return M
