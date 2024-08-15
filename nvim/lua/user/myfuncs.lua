M = {}

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

local delete_buffer = function()
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

vim.api.nvim_create_user_command("DeleteBuffer", delete_buffer, { nargs = 0 })

---Paste linewise or charwise (or even blockwise)
---@param reg string
---@param type string
---@param put string
M.paste_special = function(reg, type, put)
    local val = vim.fn.getreg(reg)
    vim.fn.setreg(reg, val, type)
    vim.fn.feedkeys(vim.api.nvim_replace_termcodes('"' .. reg .. put, true, true, true))
end

M.nav_dir = function(direction)
    local curwin = vim.api.nvim_get_current_win()
    vim.cmd.wincmd({ args = { direction } })
    local newwin = vim.api.nvim_get_current_win()
    if curwin == newwin then
        vim.fn.system("/home/oleete/.cabal/bin/xmonadctl-exe winGo-" .. direction)
    end
end

vim.api.nvim_create_user_command("NavigateLeft", function() M.nav_dir("h") end, { nargs = 0 })
vim.api.nvim_create_user_command("NavigateBottom", function() M.nav_dir("j") end, { nargs = 0 })
vim.api.nvim_create_user_command("NavigateTop", function() M.nav_dir("k") end, { nargs = 0 })
vim.api.nvim_create_user_command("NavigateRight", function() M.nav_dir("l") end, { nargs = 0 })

M.codelens_toggle = function()
    vim.g.lsp_lens_on = not vim.g.lsp_lens_on
    if vim.g.lsp_lens_on then
        vim.lsp.codelens.refresh()
    else
        vim.lsp.codelens.clear()
    end
end

return M
