local function replace_keycodes(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

function _G.commandRepeat(leader, varName)
    local key = vim.api.nvim_get_var(varName)
    local return_map
    if key == "search" then
        if leader == "]" then
            return_map = "nvv"
        else
            return_map = "Nvv"
        end
    else
        return_map = leader .. key
    end
    return replace_keycodes(return_map)
end

vim.g.dirJumps = "search"
Map({ "n", "x", "o" }, "n", "v:lua.commandRepeat(']', 'dirJumps')", { expr = true, remap = true })
Map({ "n", "x", "o" }, "N", "v:lua.commandRepeat('[', 'dirJumps')", { expr = true, remap = true })

Map({ "n", "x", "o" }, "[[", "[s", { remap = true })
Map({ "n", "x", "o" }, "]]", "]s", { remap = true })

Map({ "n", "x", "o"}, "[l", "<cmd>call v:lua.markAndGo(v:count, 'cprev', 'l')<cr>")
Map({ "n", "x", "o"}, "]l", "<cmd>call v:lua.markAndGo(v:count, 'cnext', 'l')<cr>")

vim.api.nvim_create_autocmd("BufEnter",
    {
        group = vim.api.nvim_create_augroup("diff_mappings", { clear = true }),
        callback = function()
            local bmap = function(mode, key, action) Map(mode, key, action, { buffer = vim.api.nvim_get_current_buf() }) end
            if vim.wo.diff then
                bmap({ "n", "x", "o" }, "[h", "<cmd>call v:lua.markGoCentre(v:count, 'norm! [c', 'h')<cr>")
                bmap({ "n", "x", "o" }, "]h", "<cmd>call v:lua.markGoCentre(v:count, 'norm! ]c', 'h')<cr>")
            end
        end
    }
)

-- Text object targets
function _G.git_target(count, go_forward)
    local move_cmd
    if go_forward == "true" then
        move_cmd = "Gitsigns next_hunk"
    else
        move_cmd = "Gitsigns prev_hunk"
    end
    repeat
        vim.cmd(move_cmd)
        count = count - 1
    until count <= 0
    vim.cmd("Gitsigns select_hunk")
end

function _G.mapped_targets(count, movement, selection)
    local cmd = ""
    repeat
        cmd = cmd .. movement
        count = count - 1
    until count <= 0
    cmd = cmd .. "m" .. selection
    vim.cmd([[normal ]] .. cmd)
end

function _G.plug_targets(count, movement, selection)
    local cmd = ""
    repeat
        cmd = cmd .. [[\<plug>]] .. movement
        count = count - 1
    until count <= 0

    cmd = cmd .. [[m\<plug>]] .. selection
    vim.cmd([[exe "normal ]] .. cmd .. [["]])
end

-- function _G.paragraph_end_jump(count)
--     repeat
--         vim.fn.search([[\v^$\n^\zs.+$]], "W")
--         count = count - 1
--     until count <= 0
--
--     local line_diff = vim.fn.search([[\v(^.+$\n^$|\%$)]], "Wnc") - vim.fn.line(".")
--     if line_diff > 0 then
--         vim.cmd([[normal! V]] .. line_diff .. vim.api.nvim_eval('"j\\<esc>`>"'))
--     else
--         vim.cmd("normal! " .. vim.api.nvim_eval('"V\\<esc>`>"'))
--     end
-- end
--
-- function _G.paragraph_end_jump_back(count)
--     repeat
--         vim.fn.search([[\v^.+\zs$\n^$]], "Wb")
--         count = count - 1
--     until count <= 0
--
--     local line_diff = vim.fn.line(".") - vim.fn.search([[\v(^$\n^\zs.+$|\%^)]], "bWnc")
--     if line_diff > 0 then
--         vim.cmd([[normal! V]] .. line_diff .. vim.api.nvim_eval('"k\\<esc>`>"'))
--     else
--         vim.cmd("normal! " .. vim.api.nvim_eval('"V\\<esc>`>"'))
--     end
-- end
--
-- function _G.paragraph_targets(count, around, back)
--     local move_cmd
--     if back then
--         move_cmd = function() vim.fn.search([[\v^.+\zs$\n^$]], "Wb") end
--     else
--         move_cmd = function() vim.fn.search([[\v^$\n^\zs.+$]], "W") end
--     end
--
--     repeat
--         move_cmd()
--         count = count - 1
--     until count <= 0
--
--     if around then
--         vim.cmd("norm! Vap")
--     else
--         vim.cmd("norm! Vip")
--     end
-- end

function _G.markAndGo(count, command, key)
    vim.g.dirJumps = key
    vim.cmd("norm! m`")
    repeat
        vim.cmd(command)
        count = count - 1
    until count <= 0
end

function _G.markGoCentre(count, command, key)
    vim.g.dirJumps = key
    vim.cmd("norm! m`")
    repeat
        vim.cmd(command)
        count = count - 1
    until count <= 0
    vim.cmd("norm! zz")
end
