local winclose = function() vim.cmd.wincmd({ args = { "c" } }) end

Special_types = {
    qf = { name = " Quick Fix", exit_func = winclose },
    help = { name = " help", exit_func = winclose },
    ["vim-plug"] = { name = " Vim Plug", exit_func = winclose },
    juliadoc = { name = " Julia Documentation", exit_func = winclose },
    lspinfo = { exit_func = winclose },
    tsplayground = { name = " Tree Sitter Playground", exit_func = winclose },
    ["harpoon-menu"] = { exit_func = winclose },
    toggleterm = { exit_func = winclose },
    notify = { exit_func = winclose },
    undotree = { name = "碑Undo Tree", exit_func = function() vim.cmd("UndotreeHide") end },
    NvimTree = { name = " File Tree", exit_func = winclose },
    DiffviewFileHistory = { name = " Diff History", exit_func = function() vim.cmd("DiffviewClose") end },
    DiffviewFiles = { name = " Diff Tree", exit_func = function() vim.cmd("DiffviewClose") end },
    OverseerList = { name = " Task List", exit_func = function() vim.cmd("OverseerClose") end },
    OverseerForm = { name = " Overseer Form", exit_func = winclose },
    ["dap-float"] = { exit_func = winclose },
    ["dapui_scopes"] = { name = "Scopes", exit_func = winclose },
    ["dapui_breakpoints"] = { name = "Breakpoints", exit_func = winclose },
    ["dapui_stacks"] = { name = "Stacks", exit_func = winclose },
    ["dapui_watches"] = { name = "Watches", exit_func = winclose },
    ["dap-repl"] = { name = "Debug REPL", exit_func = winclose },
    ["dapui_console"] = { name = "Debug Console", exit_func = winclose },
    ["neotest-summary"] = { name = " Test List", exit_func = winclose },
    ["vim"] = { exit_func = winclose },
    ["gitcommit"] = { name = "Git Commit Message", exit_func = winclose },
    ["noice"] = { name = "Noice", exit_func = function() vim.cmd.quit() end },
    [""] = { exit_func = winclose },
}

-- Check if a given buffer is "special"
Is_special = function(bufnr)
    local filetype = vim.bo[bufnr].filetype
    local buftype = vim.bo[bufnr].buftype
    return vim.tbl_contains(vim.tbl_keys(Special_types), filetype) or
        (buftype == "nofile" and (filetype == "" or filetype == "vim"))
end

-- Count normal windows on current tab page
function TabWinCount(tabnr)
    local tab_wins = vim.tbl_filter(function(win)
        local win_buf = vim.api.nvim_win_get_buf(win)
        if Is_special(win_buf) then
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
function LoadedBufCount()
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

function DeleteBuffer()
    local tabnr = vim.api.nvim_get_current_tabpage()
    local bufnr = vim.api.nvim_get_current_buf()

    local num_tabs = #vim.api.nvim_list_tabpages()

    -- Handle special buffers
    if Is_special(bufnr) then
        local filetype = vim.bo[bufnr].filetype
        Special_types[filetype].exit_func()
        return
    elseif vim.b[bufnr].is_diffview_file then
        vim.cmd("DiffviewFocusFiles")
        return
    end

    local num_bufs = LoadedBufCount()
    local num_tab_wins = TabWinCount(tabnr)

    if num_tab_wins > 1 then
        winclose()
    elseif num_tabs > 1 then
        vim.cmd.tabclose()
    elseif num_bufs <= 1 then
        vim.cmd.quitall()
    else
        vim.cmd.bdelete()
    end
end

vim.api.nvim_create_user_command("DeleteBuffer", DeleteBuffer, { nargs = 0 })

ZenOrFull = function()
    local num_windows = tonumber(vim.fn.systemlist([[kitty @ ls | jq ".[].tabs[] | select(.is_focused) | .windows | length"]])[1])

    if num_windows > 1 then
        vim.cmd([[silent !xdotool key --clearmodifiers "ctrl+alt+f"]])
    else
        require("mini.misc").zoom()
    end
end
vim.api.nvim_create_user_command("ZenOrFull", ZenOrFull, { nargs = 0 })

TabNext = function()
    if vim.bo[0].filetype == "toggleterm" then
        local term_id = vim.b[0].my_term_id
        local term_list = require("toggleterm.terminal").get_all(true)
        local cur_index
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
vim.api.nvim_create_user_command("TabNext", TabNext, { nargs = 0 })

TabPrev = function()
    if vim.bo[0].filetype == "toggleterm" then
        local term_id = vim.b[0].my_term_id
        local term_list = require("toggleterm.terminal").get_all(true)
        local cur_index
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
vim.api.nvim_create_user_command("TabPrev", TabPrev, { nargs = 0 })

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

VimMode = function()
    local mode_code = vim.api.nvim_get_mode().mode
    if mode_map[mode_code] == nil then
        return { mode_code, 'Normal' }
    end
    return mode_map[mode_code]
end
