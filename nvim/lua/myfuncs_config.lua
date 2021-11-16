local function replace_keycodes(str)
    return vim.api.nvim_replace_termcodes(str, true, true, true)
end

-- Repaets
-- set defaults
vim.api.nvim_set_var("panelRepeat", "q")
vim.api.nvim_set_var("DiffviewLast", "DiffviewOpen")

function _G.commandRepeat(leader, varName)
    local key = vim.api.nvim_get_var(varName)
    if key == "search" then
        if leader == "]" then
            return replace_keycodes("nzz")
        elseif leader == "[" then
            return replace_keycodes("Nzz")
        end
    end
    return replace_keycodes(leader .. key)
end

function _G.diff_repeat()
    local cmd = vim.api.nvim_get_var("DiffviewLast")
    vim.cmd(cmd)
end
-- Distant Pasting

function _G.pre_paste_away(register, paste)
    if string.find(paste, "p") then
        Paste_away_direction = "]"
        Paste_away_paste = ""
    else
        Paste_away_direction = "["
        Paste_away_paste = "!"
    end
    Paste_away_register = register
end

function _G.paste_away()
    vim.cmd([[execute "']] .. Paste_away_direction .. " put" .. Paste_away_paste .. " " .. Paste_away_register .. '"')
end

vim.api.nvim_set_keymap(
    "n",
    "<Plug>(paste-away-after)",
    [[:<c-u>call v:lua.pre_paste_away(v:register,'p')<cr>:set opfunc=v:lua.paste_away<cr>g@]],
    { noremap = true }
)
vim.api.nvim_set_keymap(
    "n",
    "<plug>(paste-away-before)",
    ":<c-u>call v:lua.pre_paste_away(v:register,'P')<cr>:set opfunc=v:lua.paste_away<cr>g@",
    { noremap = true }
)

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
local cmp = require("cmp")
_G.tab_complete = function()
    if cmp.visible() then
        return replace_keycodes("<down>")
    elseif luasnip and luasnip.expand_or_jumpable() then
        return replace_keycodes("<Plug>luasnip-expand-or-jump")
    else
        return replace_keycodes("<tab>")
    end
end

_G.s_tab_complete = function()
    if cmp.visible() then
        return replace_keycodes("<up>")
    elseif luasnip and luasnip.jumpable(-1) then
        return replace_keycodes("<Plug>luasnip-jump-prev")
    else
        return replace_keycodes("<c-d>")
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
    if #vim.fn.getbufinfo({ buflisted = true }) == 1 then
        vim.cmd([[quit]])
    elseif #vim.fn.win_findbuf(vim.fn.bufnr("%")) ~= 1 then
        vim.cmd([[wincmd c]])
    else
        require("close_buffers").delete({ type = "this" })
    end
end

function _G.KittySend(text)
    vim.fn.system("kittyrepl replterm " .. vim.b.replCommand, text)
end

function _G.sendRange(startline, endline)
    local regStore = vim.fn.getreg('"')
    local regType = vim.fn.getregtype('"')
    vim.cmd(startline .. "," .. endline .. " yank")
    KittySend(vim.fn.getreg('"'))
    vim.fn.setreg('"', regStore, regType)
end

function _G.sendLines(count)
    count = count + 1
    local regStore = vim.fn.getreg('"')
    local regType = vim.fn.getregtype('"')
    vim.cmd("normal! " .. count .. "yy")
    KittySend(vim.fn.getreg('"'))
    vim.fn.setreg('"', regStore, regType)
end

function _G.sendOp()
    local regStore = vim.fn.getreg('"')
    local regType = vim.fn.getregtype('"')
    if type == "line" then
        vim.cmd([[normal! '[V']y]])
    elseif type == "block" then
        vim.cmd([[normal! `[\<C-v>`]\y]])
    else
        vim.cmd([[normal! `[v`]y]])
    end
    KittySend(vim.fn.getreg('"') .. [[

    ]])
    vim.fn.setreg('"', regStore, regType)
    vim.cmd("normal! `z")
end

function _G.sendRegion(type)
    local regStore = vim.fn.getreg('"')
    local regType = vim.fn.getregtype('"')
    vim.cmd([[silent normal! `<]] .. type .. [[`>y]])
    KittySend(vim.fn.getreg('"'))
    vim.fn.setreg('"', regStore, regType)
    vim.cmd("normal! `>")
end

vim.api.nvim_set_keymap("x", "<Plug>(sendReg)", [[:<c-u>call v:lua.sendRegion(visualmode())<cr>]], { noremap = true })
vim.api.nvim_set_keymap("n", "<Plug>(sendOp)", [[mz:set opfunc=v:lua.sendOp<cr>g@]], { noremap = true })
