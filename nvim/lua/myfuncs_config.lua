Special_types = {
    qf = { name = " Quick Fix", exit_func = function() vim.cmd("wincmd c") end },
    help = { name = " help", exit_func = function() vim.cmd("wincmd c") end },
    ["vim-plug"] = { name = " Vim Plug", exit_func = function() vim.cmd("wincmd c") end },
    juliadoc = { name = " Julia Documentation", exit_func = function() vim.cmd("wincmd c") end },
    lspinfo = { exit_func = function() vim.cmd("wincmd c") end },
    tsplayground = { name = " Tree Sitter Playground", exit_func = function() vim.cmd("wincmd c") end },
    ["harpoon-menu"] = { exit_func = function() vim.cmd("wincmd c") end },
    toggleterm = { exit_func = function() vim.cmd("wincmd c") end },
    notify = { exit_func = function() vim.cmd("wincmd c") end },
    undotree = { name = "碑Undo Tree", exit_func = function() vim.cmd("UndotreeHide") end },
    NvimTree = { name = " File Tree", exit_func = function() vim.cmd("wincmd c") end },
    DiffviewFileHistory = { name = " Diff History", exit_func = function() vim.cmd("DiffviewClose") end },
    DiffviewFiles = { name = " Diff Tree", exit_func = function() vim.cmd("DiffviewClose") end },
    OverseerList = { name = " Task List", exit_func = function() vim.cmd("OverseerClose") end },
    OverseerForm = { name = " Overseer Form", exit_func = function() vim.cmd("wincmd c") end },
    ["neotest-summary"] = { name = " Test List", exit_func = function() vim.cmd("wincmd c") end },
    ["vim"] = { exit_func = function() vim.cmd("wincmd c") end },
    [""] = { exit_func = function() vim.cmd("wincmd c") end },
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

-- Find if the current buffer is in another window
function WinIsDuplicate(winnr)
    local is_dup
    local bufnr = vim.api.nvim_get_current_buf()
    local wins = vim.api.nvim_list_wins()
    for _, win in pairs(wins) do
        if win ~= winnr and bufnr == vim.api.nvim_win_get_buf(win) then
            is_dup = true
            break
        end
    end
    return is_dup
end

function _G.delete_buffer()
    local tabnr = vim.api.nvim_get_current_tabpage()
    local winnr = vim.api.nvim_get_current_win()
    local bufnr = vim.api.nvim_get_current_buf()

    local num_tabs = #vim.api.nvim_list_tabpages()

    -- Handle special buffers
    if Is_special(bufnr) then
        local filetype = vim.bo[bufnr].filetype

        if filetype == "" or filetype == "vim" then
            vim.cmd("stopinsert | wincmd c")
        else
            Special_types[filetype].exit_func()
        end

        return
    elseif vim.b[bufnr].is_diffview_file then
        vim.cmd("DiffviewFocusFiles")
        return
    end

    local is_dup = WinIsDuplicate(winnr)
    local num_bufs = LoadedBufCount()
    local num_tab_wins = TabWinCount(tabnr)

    -- print("num_bufs = " .. num_bufs .. ", num_tab_wins = " .. num_tab_wins .. ", is_dup = " .. tostring(is_dup))
    if num_tab_wins > 1 then
        if is_dup then
            vim.cmd([[wincmd c]])
        else
            require("close_buffers").delete({ type = "this" })
            vim.cmd([[wincmd c]])
        end
    else
        if num_tabs > 1 then
            if is_dup then
                vim.cmd([[tabclose]])
            else
                require("close_buffers").delete({ type = "this" })
                vim.cmd([[tabclose]])
            end
        else
            if num_bufs > 1 then
                require("close_buffers").delete({ type = "this" })
            else
                vim.cmd([[quitall]])
            end
        end

    end
end

vim.cmd([[command! DeleteBuffer call v:lua.delete_buffer()]])

ZenOrFull = function()
    local handle = io.popen([[kitty @ ls | jq ".[].tabs[] | select(.is_focused) | .windows | length"]])
    if not handle then return end
    local num_windows = tonumber(handle:read("*a"))
    handle:close()

    if num_windows > 1 then
        vim.cmd([[silent !xdotool key --clearmodifiers "ctrl+alt+f"]])
    else
        zoom()
        -- local num_tabs = #vim.api.nvim_list_tabpages()
        -- local num_tab_wins = TabWinCount(vim.api.nvim_get_current_tabpage())
        -- if num_tab_wins <= 1 and num_tabs > 1 then
        --     vim.cmd("tabclose")
        -- else
        --     vim.cmd("tab split")
        -- end
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
        local next_index = cur_index == #term_list and 1 or cur_index+1
        local cur_win = vim.api.nvim_get_current_win()
        vim.api.nvim_win_set_buf(cur_win, term_list[next_index].bufnr)
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
        local next_index = cur_index == 1 and #term_list or cur_index-1
        local cur_win = vim.api.nvim_get_current_win()
        vim.api.nvim_win_set_buf(cur_win, term_list[next_index].bufnr)
    else
        vim.cmd("tabprevious")
    end
end
vim.api.nvim_create_user_command("TabPrev", TabPrev, { nargs = 0 })
