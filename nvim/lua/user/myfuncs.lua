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
    query = { name = " TS Query", exit_func = winclose },
    ["harpoon-menu"] = { exit_func = winclose },
    toggleterm = { exit_func = winclose },
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
    ["vim"] = { exit_func = winclose },
    ["gitcommit"] = { name = "Git Commit Message", exit_func = winclose },
    ["noice"] = { name = "Noice", exit_func = function() vim.cmd.quit() end },
    ["mason"] = { exit_func = winclose },
    ["null-ls-info"] = { exit_func = winclose },
    ["Glance"] = { exit_func = require('glance').actions.close },
    [""] = { exit_func = winclose },
}

-- Check if a given buffer is "special"
M.is_special = function(bufnr)
    local filetype = vim.bo[bufnr].filetype
    local buftype = vim.bo[bufnr].buftype
    return vim.tbl_contains(vim.tbl_keys(M.special_types), filetype) or
        (buftype == "nofile" and (filetype == "" or filetype == "vim"))
end

-- Count normal windows on current tab page
local tab_win_count = function(tabnr)
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
    local num_tab_wins = #tab_wins
    return num_tab_wins
end

-- Count loaded normal buffers
local loaded_buf_count = function()
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
    local num_bufs = #bufnrs
    return num_bufs
end

local delete_buffer = function()
    local tabnr = vim.api.nvim_get_current_tabpage()
    local bufnr = vim.api.nvim_get_current_buf()

    local num_tabs = #vim.api.nvim_list_tabpages()

    -- Handle special buffers
    if M.is_special(bufnr) then
        local filetype = vim.bo[bufnr].filetype
        M.special_types[filetype].exit_func()
        return
    elseif vim.b[bufnr].is_diffview_file then
        vim.b.is_diffview_file = false
        vim.cmd("DiffviewFocusFiles")
        return
    end

    local num_bufs = loaded_buf_count()

    local num_tab_wins = tab_win_count(tabnr)

    if num_tab_wins > 1 then
        winclose()
    elseif num_tabs > 1 then
        vim.cmd.tabclose()
    elseif num_bufs <= 1 then
        vim.cmd.quitall()
    else
        vim.cmd.Bdelete()
    end
end

vim.api.nvim_create_user_command("DeleteBuffer", delete_buffer, { nargs = 0 })

local zen_or_full = function()
    local num_windows = tonumber(vim.fn.systemlist([[kitty @ ls | jq ".[].tabs[] | select(.is_focused) | .windows | length"]])
        [1])

    if num_windows > 1 then
        vim.cmd([[silent !xdotool key --clearmodifiers "ctrl+alt+f"]])
    else
        require("mini.misc").zoom()
    end
end
vim.api.nvim_create_user_command("ZenOrFull", zen_or_full, { nargs = 0 })

local tab_next = function()
    if vim.bo[0].filetype == "toggleterm" then
        local term_id = vim.b[0].my_term_id
        local term_list = require("toggleterm.terminal").get_all(true)
        local cur_index = 1
        for i, term in pairs(term_list) do
            if tonumber(term_id) == term.id then
                cur_index = i
            end
        end
        local next_index = cur_index == #term_list and 1 or cur_index + 1
        local cur_win = vim.api.nvim_get_current_win()
        vim.api.nvim_win_set_buf(cur_win, term_list[next_index].bufnr)
        OTerm = term_list[next_index]
        OTerm.window = vim.api.nvim_get_current_win()
    else
        vim.cmd("tabnext")
    end
end
vim.api.nvim_create_user_command("TabNext", tab_next, { nargs = 0 })

local tab_prev = function()
    if vim.bo[0].filetype == "toggleterm" then
        local term_id = vim.b[0].my_term_id
        local term_list = require("toggleterm.terminal").get_all(true)
        local cur_index = #term_list
        for i, term in pairs(term_list) do
            if tonumber(term_id) == term.id then
                cur_index = i
            end
        end
        local next_index = cur_index == 1 and #term_list or cur_index - 1
        local cur_win = vim.api.nvim_get_current_win()
        vim.api.nvim_win_set_buf(cur_win, term_list[next_index].bufnr)
        OTerm = term_list[next_index]
        OTerm.window = vim.api.nvim_get_current_win()
    else
        vim.cmd("tabprevious")
    end
end
vim.api.nvim_create_user_command("TabPrev", tab_prev, { nargs = 0 })

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
    ['t'] = { 'TERMINAL', 'Command' },
}

M.vim_mode = function()
    local mode_code = vim.api.nvim_get_mode().mode
    if mode_map[mode_code] == nil then
        return { mode_code, 'Normal' }
    end
    return mode_map[mode_code]
end

M.paste_special = function(reg, type, put)
    local val = vim.fn.getreg(reg)
    vim.fn.setreg(reg, val, type)
    vim.fn.feedkeys(vim.api.nvim_replace_termcodes('"' .. reg .. put, true, true, true))
end

M.trash_put = function()
    vim.cmd("!trash-put %")
    vim.cmd.bdelete()
end

return M
