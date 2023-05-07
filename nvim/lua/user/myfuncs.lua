M = {}

local winclose = function() vim.cmd.wincmd({ args = { "c" } }) end

M.special_types = {
    lazy = { exit_func = winclose },
    qf = { name = " QuickFix", exit_func = winclose },
    help = { name = " Help", exit_func = winclose },
    ["vim-plug"] = { name = " Plugs", exit_func = winclose },
    juliadoc = { name = " Docs", exit_func = winclose },
    lspinfo = { exit_func = winclose },
    tsplayground = { name = " TSPlayground", exit_func = winclose },
    ["harpoon-menu"] = { exit_func = winclose },
    notify = { exit_func = winclose },
    undotree = { name = "碑Undos", exit_func = function() vim.cmd("UndotreeHide") end },
    DiffviewFileHistory = { name = " Diffs", exit_func = function() vim.cmd("DiffviewClose") end },
    DiffviewFiles = { name = " Diffs", exit_func = function() vim.cmd("DiffviewClose") end },
    OverseerList = { name = " Tasks", exit_func = function() vim.cmd("OverseerClose") end },
    OverseerForm = { exit_func = winclose },
    ["dap-float"] = { exit_func = winclose },
    ["dapui_scopes"] = { name = "Scopes", exit_func = winclose },
    ["dapui_breakpoints"] = { name = "Breakpoints", exit_func = winclose },
    ["dapui_stacks"] = { name = "Stacks", exit_func = winclose },
    ["dapui_watches"] = { name = "Watches", exit_func = winclose },
    ["dap-repl"] = { name = "Debug REPL", exit_func = winclose },
    ["dapui_console"] = { name = "Debug Console", exit_func = winclose },
    ["neotest-summary"] = { name = " Tests", exit_func = winclose },
    ["neotest-output-panel"] = { name = " Test Output", exit_func = winclose },
    ["neotest-output"] = { name = " Tests", exit_func = winclose },
    ["gitcommit"] = { name = "Git Commit Message", exit_func = winclose },
    ["noice"] = {  exit_func = function() vim.cmd.quit() end },
    ["mason"] = { exit_func = winclose },
    ["null-ls-info"] = { exit_func = winclose },
    ["Glance"] = { exit_func = require('glance').actions.close },
    asm = { name = "Compiler Explorer", exit_func = winclose },
}


M.is_special = function(bufnr)
    local filetype = vim.bo[bufnr].filetype
    local buftype = vim.bo[bufnr].buftype
    if buftype == "terminal" and filetype ~= "neotest-output-panel" and filetype ~= "neotest-output" then return false end
    return vim.tbl_contains(vim.tbl_keys(M.special_types), filetype)
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
    elseif vim.b[bufnr].is_diffview_file then
        vim.b.is_diffview_file = false
        vim.cmd("DiffviewFocusFiles")
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

vim.api.nvim_create_user_command("ZenOrFull", require("mini.misc").zoom, { nargs = 0 })

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

M.trash_put = function()
    vim.cmd("!trash-put %")
    vim.cmd.bdelete()
end

local openterm = function()
    vim.cmd("silent split")
    vim.cmd("OverseerRun Fish")
end
vim.api.nvim_create_user_command("OpenTerm", openterm, { nargs = 0 })

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

return M
