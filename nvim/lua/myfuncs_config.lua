local function replace_keycodes(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

-- Repaets
-- set defaults
vim.api.nvim_set_var("dirJumps", "f")
vim.api.nvim_set_var("panelRepeat", "x")
vim.api.nvim_set_var("DiffviewLast", "DiffviewOpen")

function _G.commandRepeat(leader, varName)
    local jump = vim.api.nvim_get_var(varName)
    return vim.api.nvim_replace_termcodes(leader .. jump, true, true, true)
end

function _G.diff_repeat()
    local cmd = vim.api.nvim_get_var("DiffviewLast")
    vim.cmd(cmd)
end

-- Text object targets
function _G.ts_target(count, object)
    vim.cmd("TSTextobjectGotoNextStart " .. object)
    count = count - 1
    while count > 0 do
        vim.cmd("TSTextobjectGotoNextStart " .. object)
        count = count - 1
    end
    vim.cmd("TSTextobjectSelect " .. object)
end
function _G.ts_target_back(count, object)
    vim.cmd("TSTextobjectGotoPreviousEnd " .. object)
    count = count - 1
    while count > 0 do
        vim.cmd("TSTextobjectGotoPreviousEnd " .. object)
        count = count - 1
    end
    vim.cmd("TSTextobjectGotoPreviousStart " .. object)
    vim.cmd("TSTextobjectSelect " .. object)
end

function _G.git_target(count, forward)
    local move_cmd
    if forward == "true" then
        move_cmd = "Gitsigns next_hunk"
    else
        move_cmd = "Gitsigns prev_hunk"
    end
    vim.cmd(move_cmd)
    count = count - 1
    while count > 0 do
        vim.cmd(move_cmd)
        count = count - 1
    end
    vim.cmd("Gitsigns select_hunk")
end

function _G.mapped_targets(count, movement, selection)
    local cmd = movement
    count = count - 1
    while count > 0 do
        cmd = cmd .. movement
        count = count - 1
    end
    cmd = cmd .. "v" .. selection
    vim.cmd([[normal ]] .. cmd)
end
function _G.mapped_targets_back(count, movement, end_movement, selection)
    local cmd = movement
    count = count - 1
    while count > 0 do
        cmd = cmd .. movement
        count = count - 1
    end
    cmd = cmd .. end_movement
    cmd = cmd .. "v" .. selection
    vim.cmd([[normal ]] .. cmd)
end

function _G.plug_targets(count, movement, selection)
    local cmd = [[\<plug>]] .. movement
    count = count - 1
    while count > 0 do
        cmd = cmd .. [[\<plug>]] .. movement
        count = count - 1
    end
    cmd = cmd .. [[v\<plug>]] .. selection
    vim.cmd([[exe "normal ]] .. cmd .. [["]])
end
function _G.plug_targets_back(count, movement, end_movement, selection)
    local cmd = [[\<plug>]] .. movement
    count = count - 1
    while count > 0 do
        cmd = cmd .. [[\<plug>]] .. movement
        count = count - 1
    end
    cmd = [[\<plug>]] .. end_movement
    cmd = cmd .. [[v\<plug>]] .. selection
    vim.cmd([[exe "normal ]] .. cmd .. [["]])
end

function _G.paragraph_targets(count, around)
    vim.fn.search([[\v^$\n^\zs.+$]], "W")
    count = count - 1
    while count > 0 do
        vim.fn.search([[\v^$\n^\zs.+$]], "W")
        count = count - 1
    end
    local line_diff
    if around == 1 then
        line_diff = vim.fn.search([[\v(^$\n^.+$|\%$)]], "Wnc") - vim.fn.line(".")
    else
        line_diff = vim.fn.search([[\v(^.+$\n^$|\%$)]], "Wnc") - vim.fn.line(".")
    end
    if line_diff > 0 then
        vim.cmd([[normal! V]] .. line_diff .. "j")
    else
        vim.cmd([[normal! V]])
    end
end

function _G.paragraph_targets_back(count, around)
    vim.fn.search([[\v^.+\zs$\n^$]], "Wb")
    count = count - 1
    while count > 0 do
        vim.fn.search([[\v^.+\zs$\n^$]], "Wb")
        count = count - 1
    end
    local line_diff
    if around == 1 then
        line_diff = vim.fn.line(".") - vim.fn.search([[\v(^.+$\n^$|\%^)]], "bWnc")
    else
        line_diff = vim.fn.line(".") - vim.fn.search([[\v(^$\n^\zs.+$|\%^)]], "bWnc")
    end
    if line_diff > 0 then
        vim.cmd([[normal! V]] .. line_diff .. "k")
    else
        vim.cmd([[normal! V]])
    end
end

-- Distant Pasting

function _G.pre_paste_away(register, paste)
    if string.find(paste, "p") then
        Paste_away_direction= "]"
        Paste_away_paste = ""
    else
        Paste_away_direction= "["
        Paste_away_paste = "!"
    end
    Paste_away_register = register
end

function _G.paste_away()
    vim.cmd([[execute "']] .. Paste_away_direction .. " put" .. Paste_away_paste .. " " .. Paste_away_register .. '"')
end

vim.api.nvim_set_keymap("n", "<Plug>(paste-away-after)", [[:<c-u>call v:lua.pre_paste_away(v:register,'p')<cr>:set opfunc=v:lua.paste_away<cr>g@]], {noremap=true})
vim.api.nvim_set_keymap("n", "<plug>(paste-away-before)", ":<c-u>call v:lua.pre_paste_away(v:register,'P')<cr>:set opfunc=v:lua.paste_away<cr>g@", {noremap=true})

-- Telescope

local action_state = require("telescope.actions.state")

local open_dif = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    local cmd = "DiffviewOpen " .. value
    vim.api.nvim_set_var("DiffviewLast", cmd)
    vim.cmd(cmd)
end
local open_dif_mergebase = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    local cmd = "DiffviewOpen ..." .. value
    vim.api.nvim_set_var("DiffviewLast", cmd)
    vim.cmd(cmd)
end
local open_single_dif = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    local cmd = "DiffviewOpen " .. value .. "~1.." .. value
    vim.api.nvim_set_var("DiffviewLast", cmd)
    vim.cmd(cmd)
end
local change_gitsign_base = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    vim.api.nvim_win_close(0, true)
    local cmd = "Gitsigns change_base " .. value
    vim.cmd(cmd)
end

function _G.gitsign_change_base()
    require("telescope.builtin").git_commits({
        attach_mappings = function(_, map)
            map("n", "<cr>", change_gitsign_base)
            map("i", "<cr>", change_gitsign_base)
            return true
        end,
    })
end
function _G.gitsign_bchange_base()
    require("telescope.builtin").git_bcommits({
        attach_mappings = function(_, map)
            map("n", "<cr>", change_gitsign_base)
            map("i", "<cr>", change_gitsign_base)
            return true
        end,
    })
end

function _G.git_commits_againsthead()
    require("telescope.builtin").git_commits({
        attach_mappings = function(_, map)
            map("n", "<cr>", open_dif)
            map("i", "<cr>", open_dif)
            return true
        end,
    })
end
function _G.git_commits_onechange()
    require("telescope.builtin").git_commits({
        attach_mappings = function(_, map)
            map("n", "<cr>", open_single_dif)
            map("i", "<cr>", open_single_dif)
            return true
        end,
    })
end

function _G.git_branch_dif()
    require("telescope.builtin").git_branches({
        attach_mappings = function(_, map)
            map("n", "<cr>", open_dif)
            map("i", "<cr>", open_dif)
            return true
        end,
    })
end
function _G.git_branch_mergebase()
    require("telescope.builtin").git_branches({
        attach_mappings = function(_, map)
            map("n", "<cr>", open_dif_mergebase)
            map("i", "<cr>", open_dif_mergebase)
            return true
        end,
    })
end

function _G.project_files()
    local opts = {} -- define here if you want to define something
    local ok = pcall(require("telescope.builtin").git_files, opts)
    if not ok then
        require("telescope.builtin").find_files(opts)
    end
end

-- Compleation functions

local luasnip = require("luasnip")
_G.tab_complete = function()
    if luasnip and luasnip.expand_or_jumpable() then
        return replace_keycodes("<Plug>luasnip-expand-or-jump")
    elseif neogen.jumpable() then
        return replace_keycodes("<cmd>lua require('neogen').jump_next()<CR>")
    else
        return replace_keycodes("<plug>(TaboutMulti)")
    end
end

_G.s_tab_complete = function()
    if luasnip and luasnip.jumpable(-1) then
        return replace_keycodes("<Plug>luasnip-jump-prev")
    else
        return replace_keycodes("<plug>(TaboutBackMulti)")
    end
end

_G.cmp_toggle = function()
    if require("cmp").visible() then
        return replace_keycodes([[<cmd>lua require("cmp").close()<cr>]])
    else
        return replace_keycodes([[<cmd>lua require("cmp").complete()<cr>]])
    end
end

_G.cmp_esc = function()
    if require("cmp").visible() then
        return replace_keycodes([[<cmd>lua require("cmp").close()<cr>]])
    else
        return replace_keycodes("<esc>")
    end
end

-- Toggle Quickfix list
function _G.toggle_qflist()
    local qf_open = false
    for _, win in pairs(vim.fn.getwininfo()) do
        if win["quickfix"] == 1 then
            qf_open = true
        end
    end
    if qf_open == true then
        vim.cmd("cclose")
    elseif not vim.tbl_isempty(vim.fn.getqflist()) then
        vim.cmd("copen")
    end
end
function _G.toggle_loclist()
    local loc_open = false
    for _, win in pairs(vim.fn.getwininfo()) do
        if win["loclist"] == 1 then
            loc_open = true
        end
    end
    if loc_open == true then
        vim.cmd("lclose")
    else
        vim.cmd("lopen")
    end
end

function _G.delete_buffer()
  if #vim.fn.getbufinfo({buflisted = true}) == 1 then
    vim.cmd([[quit]])
  else
    require('close_buffers').delete({type = 'this'})
  end
end
