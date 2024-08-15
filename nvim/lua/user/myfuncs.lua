M = {}

local winclose = function() vim.cmd.wincmd({ args = { "c" } }) end
local edgy_left = function() require("edgy").close("left") end
local edgy_bot = function() require("edgy").close("bottom") end
local diffclose = function() vim.cmd.DiffviewClose() end
local troubleclose = function() require("trouble").close() end
local edgyclose = function() require("edgy").close() end
M.special_types = {
    lazy = { exit_func = winclose },
    qf = { exit_func = winclose },
    trouble = { exit_func = troubleclose },
    help = { exit_func = edgy_left },
    lspinfo = { exit_func = winclose },
    notify = { exit_func = winclose },
    DiffviewFileHistory = { exit_func = diffclose },
    DiffviewFiles = { exit_func = diffclose },
    edgy = { exit_func = edgyclose },
    ["dap-float"] = { exit_func = winclose },
    ["dapui_scopes"] = { exit_func = winclose },
    ["dapui_breakpoints"] = { exit_func = winclose },
    ["dapui_stacks"] = { exit_func = winclose },
    ["dapui_watches"] = { exit_func = winclose },
    ["dap-repl"] = { exit_func = winclose },
    ["dapui_console"] = { exit_func = winclose },
    ["gitcommit"] = { exit_func = winclose },
    ["NoiceHistory"] = { exit_func = edgy_bot },
    ["mason"] = { exit_func = winclose },
    ["null-ls-info"] = { exit_func = winclose },
    asm = { exit_func = winclose },
}

M.is_special = function(bufnr)
    local filetype = vim.bo[bufnr].filetype
    local buftype = vim.bo[bufnr].buftype
    return buftype == "terminal" or vim.tbl_contains(vim.tbl_keys(M.special_types), filetype)
end

---Count normal windows on current tab page
---@param tabnr number
---@return number[]
local tab_win_bufnrs = function(tabnr)
    local tab_wins = vim.tbl_filter(function(win)
        local win_buf = vim.api.nvim_win_get_buf(win)
        if M.is_special(win_buf) then
            return false
        end
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

    -- Handle special buffers
    if M.is_special(bufnr) then
        local filetype = vim.bo[bufnr].filetype
        if filetype == "" then
            vim.cmd.quit()
        else
            M.special_types[filetype].exit_func()
        end
        return
    elseif vim.bo[bufnr].buftype == "terminal" then
        winclose()
        return
    end

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

local mode_map = {
    ['n'] = { 'NORMAL', 'Normal' },
    ['no'] = { 'O-PENDING', 'Visual' },
    ['nov'] = { 'O-PENDING', 'Visual' },
    ['noV'] = { 'O-PENDING', 'Visual' },
    ['no'] = { 'O-PENDING', 'Visual' },
    ['nt'] = { 'T-NORMAL', 'Normal' },
    ['niI'] = { 'NORMAL', 'Normal' },
    ['niR'] = { 'NORMAL', 'Normal' },
    ['niV'] = { 'NORMAL', 'Normal' },
    ['v'] = { 'VISUAL', 'Visual' },
    ['V'] = { 'V-LINE', 'Visual' },
    [''] = { 'V-BLOCK', 'Visual' },
    ['s'] = { 'SELECT', 'Visual' },
    ['S'] = { 'S-LINE', 'Visual' },
    [''] = { 'S-BLOCK', 'Visual' },
    ['i'] = { 'INSERT', 'Insert' },
    ['ic'] = { 'INSERT', 'Insert' },
    ['ix'] = { 'INSERT', 'Insert' },
    ['R'] = { 'REPLACE', 'Replace' },
    ['Rc'] = { 'REPLACE', 'Replace' },
    ['Rv'] = { 'V-REPLACE', 'Normal' },
    ['Rx'] = { 'REPLACE', 'Normal' },
    ['Rvc'] = { 'V-REPLACE', 'Replace' },
    ['Rvx'] = { 'V-REPLACE', 'Replace' },
    ['c'] = { 'COMMAND', 'Command' },
    ['cv'] = { 'EX', 'Command' },
    ['ce'] = { 'EX', 'Command' },
    ['r'] = { 'REPLACE', 'Replace' },
    ['rm'] = { 'MORE', 'Normal' },
    ['r?'] = { 'CONFIRM', 'Normal' },
    ['!'] = { 'SHELL', 'Normal' },
    ['t'] = { 'TERMINAL', 'Terminal' },
}

---Return the current mode
---@return string[]
M.vim_mode = function()
    local mode_code = vim.api.nvim_get_mode().mode
    if mode_map[mode_code] == nil then
        return { mode_code, 'Normal' }
    end
    return mode_map[mode_code]
end

---Paste linewise or charwise (or even blockwise)
---@param reg string
---@param type string
---@param put string
M.paste_special = function(reg, type, put)
    local val = vim.fn.getreg(reg)
    vim.fn.setreg(reg, val, type)
    vim.fn.feedkeys(vim.api.nvim_replace_termcodes('"' .. reg .. put, true, true, true))
end

M.toggle_autowrap = function()
    if vim.b[0].to_wrap == nil or vim.b[0].to_wrap == true then
        vim.b[0].to_wrap = false
        vim.b[0].old_wrap = vim.b[0].textwidth
        vim.b[0].textwidth = 0
        if vim.g.textwidth ~= 0 then
            vim.g.old_text_width = vim.g.textwidth
            vim.g.textwidth = 0
        end
    else
        vim.b[0].to_wrap = true
        vim.b[0].textwidth = vim.b[0].old_wrap
        vim.g.textwidth = vim.g.old_text_width
    end
end

M.nav_dir = function(direction)
    local curwin = vim.api.nvim_get_current_win()
    vim.cmd.wincmd({ args = { direction } })
    if curwin == vim.api.nvim_get_current_win() then
        vim.fn.system("/home/oleete/.cabal/bin/xmonadctl-exe winGo-" .. direction)
    end
end


vim.api.nvim_create_user_command("NavigateLeft", function() M.nav_dir("h") end, { nargs = 0 })
vim.api.nvim_create_user_command("NavigateBottom", function() M.nav_dir("j") end, { nargs = 0 })
vim.api.nvim_create_user_command("NavigateTop", function() M.nav_dir("k") end, { nargs = 0 })
vim.api.nvim_create_user_command("NavigateRight", function() M.nav_dir("l") end, { nargs = 0 })

M.toggle_noice = function()
    local oldbufnr = vim.api.nvim_get_current_buf()
    for _, winnr in ipairs(vim.api.nvim_tabpage_list_wins(0)) do
        if vim.api.nvim_win_is_valid(winnr) then
            local bufnr = vim.api.nvim_win_get_buf(winnr)
            if vim.bo[bufnr].filetype == "NoiceHistory" then
                vim.api.nvim_win_close(winnr, true)
            end
        end
    end
    require("noice").cmd("history")
    if oldbufnr ~= vim.api.nvim_get_current_buf() then
        vim.bo.filetype = "NoiceHistory"
    end
end


TabNext = function()
    if vim.bo[0].filetype == "neo-tree" then
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(">", true, true, true), "m", false)
    else
        vim.cmd("tabnext")
    end
end
vim.api.nvim_create_user_command("TabNext", TabNext, { nargs = 0 })

TabPrev = function()
    if vim.bo[0].filetype == "neo-tree" then
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<", true, true, true), "m", false)
    else
        vim.cmd("tabprevious")
    end
end
vim.api.nvim_create_user_command("TabPrev", TabPrev, { nargs = 0 })

M.neogit = function()
    require("neogit").open()
    vim.cmd.LualineRenameTab({ args = { "Neogit" } })
end

M.codelens_toggle = function()
    vim.g.lsp_lens_on = not vim.g.lsp_lens_on
    if vim.g.lsp_lens_on then
        vim.lsp.codelens.refresh()
    else
        vim.lsp.codelens.clear()
    end
end

M.toggle_quickfix = function()
    local qf_exists = false
    for _, win in pairs(vim.fn.getwininfo()) do
        if win["quickfix"] == 1 then
            qf_exists = true
        end
    end
    if qf_exists == true then
        vim.cmd.cclose()
    else
        vim.cmd.copen()
    end
end

return M
