local M = {}

local winclose = function() vim.cmd.wincmd({ args = { "c" } }) end

---Count normal windows on current tab page
---@param tabnr number
---@return number[]
local tab_win_bufnrs = function(tabnr)
    local tab_wins = vim.tbl_filter(function(win)
        local win_buf = vim.api.nvim_win_get_buf(win)
        if 1 ~= vim.fn.buflisted(win_buf) then
            return false
        end
        return true
    end, vim.api.nvim_tabpage_list_wins(tabnr))
    return tab_wins
end

---Count loaded normal buffers
---@return number[]
local loaded_bufnrs = function()
    local bufnrs = vim.tbl_filter(function(b)
        if 1 ~= vim.fn.buflisted(b) then
            return false
        end
        -- only hide unloaded buffers if opts.show_all_buffers is false, keep them listed if true or nil
        if not vim.api.nvim_buf_is_loaded(b) then
            return false
        end
        return true
    end, vim.api.nvim_list_bufs())
    return bufnrs
end

M.delete_buffer = function()
    local tabnr = vim.api.nvim_get_current_tabpage()
    local bufnr = vim.api.nvim_get_current_buf()
    local num_tabs = #vim.api.nvim_list_tabpages()
    local bufs = loaded_bufnrs()
    local tab_wins = tab_win_bufnrs(tabnr)

    if #tab_wins > 1 then
        winclose()
    elseif num_tabs > 1 then
        if bufs[1] == bufnr then
            vim.cmd.tabclose()
        else
            winclose()
        end
    elseif #bufs <= 1 then
        if bufs[1] == bufnr then
            vim.cmd.quitall()
        else
            winclose()
        end
    else
        require("mini.bufremove").delete()
    end
end


M.nav_dir = function(direction)
    local curwin = vim.api.nvim_get_current_win()
    vim.cmd.wincmd({ args = { direction } })
    local newwin = vim.api.nvim_get_current_win()
    if curwin == newwin then
        vim.fn.system("/home/oleete/.cabal/bin/xmonadctl-exe winGo-" .. direction)
    end
end

---Run an action on a snacks picker trouble list, using the most recently set of the two lists.
---@param action "open" | "close" | "toggle" The action to take with the list
M.trouble_snacks = function(action)
    local mode = require("trouble.sources.snacks").mode()
    local not_mode = mode == "snacks" and "snacks_files" or "snacks"
    require("trouble").close(not_mode)

    local o_or_c
    if action == "toggle" then
        o_or_c = require("trouble").is_open(mode) and "close" or "open"
    else
        o_or_c = action
    end

    if o_or_c == "open" then
        require("trouble").open(mode)
        ---@diagnostic disable-next-line: missing-parameter, param-type-mismatch
        require("trouble").focus(mode)
    elseif o_or_c == "close" then
        require("trouble").close(mode)
    end
end

return M
