Special_types = {
    qf = { name = "Quick Fix", exit_func = function() vim.cmd("wincmd c") end },
    help = { name = "help", exit_func = function() vim.cmd("wincmd c") end },
    ["vim-plug"] = { name = "Vim Plug", exit_func = function() vim.cmd("wincmd c") end },
    juliadoc = { name = "Julia Documentation", exit_func = function() vim.cmd("wincmd c") end },
    lspinfo = { exit_func = function() vim.cmd("wincmd c") end },
    tsplayground = { name = "Tree Sitter Playground", exit_func = function() vim.cmd("wincmd c") end },
    ["harpoon-menu"] = { exit_func = function() vim.cmd("wincmd c") end },
    toggleterm = { exit_func = function() vim.cmd("wincmd c") end },
    notify = { exit_func = function() vim.cmd("wincmd c") end },
    undotree = { name = "Undo Tree", exit_func = function() vim.cmd("UndotreeHide") end },
    NvimTree = { name = "File Tree", exit_func = function() vim.cmd("wincmd c") end },
    DiffviewFileHistory = { name = "Diff History", exit_func = function() vim.cmd("DiffviewClose") end },
    DiffviewFiles = { name = "Diff Tree", exit_func = function() vim.cmd("DiffviewClose") end },
    ["vim"] = { exit_func = function() vim.cmd("wincmd c") end },
    [""] = { exit_func = function() vim.cmd("wincmd c") end },
}

Is_special = function(bufnr)
    local filetype = vim.bo[bufnr].filetype
    local buftype = vim.bo[bufnr].buftype
    return vim.tbl_contains(vim.tbl_keys(Special_types), filetype) or
        (buftype == "nofile" and (filetype == "" or filetype == "vim"))
end

function _G.delete_buffer()
    local tabnr = vim.api.nvim_get_current_tabpage()
    local winnr = vim.api.nvim_get_current_win()
    local bufnr = vim.api.nvim_get_current_buf()

    local num_tabs = #vim.api.nvim_list_tabpages()

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

    -- Count loaded normal buffers
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

    -- Find if the current buffer is in another window
    local wins = vim.api.nvim_list_wins()
    local is_dup = false
    for _, win in pairs(wins) do
        if win ~= winnr and bufnr == vim.api.nvim_win_get_buf(win) then
            is_dup = true
            break
        end
    end

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

    -- Count normal windows on current tab page
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
