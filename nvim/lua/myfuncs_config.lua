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
        Paste_away_paste = "p"
    else
        Paste_away_direction = "["
        Paste_away_paste = "P"
    end
    Paste_away_regtype = vim.fn.getregtype(register)
    Paste_away_register = register
end

function _G.paste_away()
    -- vim.cmd("normal! `" .. Paste_away_direction .. '"' .. Paste_away_register .. Paste_away_paste)
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

function _G.delete_buffer()
    if vim.bo.filetype == "man" then
        vim.cmd([[bdelete]])
    elseif #vim.fn.getbufinfo({ buflisted = true }) == 1 then
        vim.cmd([[quit]])
    -- elseif #vim.fn.win_findbuf(vim.fn.bufnr("%")) ~= 1 then
    elseif vim.fn.winnr("$") ~= 2 then
        vim.cmd([[wincmd c]])
    else
        require("close_buffers").delete({ type = "this" })
    end
end

vim.cmd([[command DeleteBuffer call v:lua.delete_buffer()]])

function _G.KittySend(text)
    vim.fn.system("kittyrepl " .. vim.b[0].replName .. " " .. vim.b[0].replCommand, text)
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

-- https://www.reddit.com/r/neovim/comments/nrz9hp/can_i_close_all_floating_windows_without_closing/h0lg5m1/
function _G.closeFloatWins()
  for _, win in ipairs(vim.api.nvim_list_wins()) do
    local config = vim.api.nvim_win_get_config(win)
    if config.relative ~= '' then
      vim.api.nvim_win_close(win, false)
    end
  end
end
