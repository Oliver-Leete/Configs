Term_on_open = function(term)
    vim.wo[term.window].signcolumn = "no"
end

require("toggleterm").setup({
    size = function()
        local height = vim.api.nvim_list_uis()[1].height
        return math.floor(height * 0.3)
    end,
    on_open = function(term)
        Term_on_open(term)
    end,
    shade_terminals = false,
    hide_numbers = true,
    start_in_insert = true,
    persist_mode = false,
    insert_mappings = true,
    terminal_mappings = true,
    persist_size = true,
    direction = "horizontal",
    close_on_exit = true,
    shell = "fish",
    float_opts = {
        border = "curved",
    }
})

Terminal = require("toggleterm.terminal").Terminal
BackgroundTerm = Terminal:new({
    on_open = function(term) Term_on_open(term); vim.b[0].my_term_title = "Background" end,
    runnable = { source = "Julia", name = "Julia Doc Server", func = function() JuliaLiveDocs:set_toggle(4) end }
})

Harp_Term_1 = Terminal:new()
Harp_Term_2 = Terminal:new({ on_open = function(term) Term_on_open(term); vim.b[0].my_term_title = "One Shots" end })
Harp_Term_3 = Terminal:new()
Harp_Term_4 = BackgroundTerm

function Terminal:set_harp(term_num)
    if term_num == 1 then
        if Harp_Term_1 ~= self then
            if Harp_Term_1:is_open() then
                Harp_Term_1:close()
            end
            Harp_Term_1 = self
        end
    elseif term_num == 2 then
        if Harp_Term_2 ~= self then
            if Harp_Term_2:is_open() then
                Harp_Term_2:close()
            end
            Harp_Term_2 = self
        end
    elseif term_num == 3 then
        if Harp_Term_3 ~= self then
            if Harp_Term_3:is_open() then
                Harp_Term_3:close()
            end
            Harp_Term_3 = self
        end
    elseif term_num == 4 then
        if Harp_Term_4 ~= self then
            if Harp_Term_4:is_open() then
                Harp_Term_4:close()
            end
            Harp_Term_4 = self
        end
    end
end

function Terminal:set_toggle(term_num)
    self:set_harp(term_num)
    self:toggle()
end

function Terminal:send_open(cmd, go_back, term_num)
    if term_num then
        self:set_harp(term_num)
    end

    if not self:is_open() then
        self:toggle()
    end
    self:send(cmd, false)
end

Background_Term_List = {
    { source = "default", name = "default", func = function() BackgroundTerm:set_toggle(4) end },
}

function Terminal:open_add()
    if not Background_Term_List[self.runnable.name] then
        Background_Term_List[self] = self.runnable
    end
    self:set_harp(4)
    if not self:is_open() then
        self:toggle()
    end
end

NvimLogTerm = Terminal:new({
    on_open = function(term) Term_on_open(term); vim.b[0].my_term_title = "Neovim Log" end,
    cmd = "tail --follow --retry ~/.local/state/nvim/log | less -S",
    runnable = { source = "log", name = "Neovim Log", func = function() NvimLogTerm:set_toggle(4) end },
})

LspLogTerm = Terminal:new({
    on_open = function(term) Term_on_open(term); vim.b[0].my_term_title = "LSP Log" end,
    cmd = "tail --follow --retry ~/.local/state/nvim/lsp.log | less -S",
    runnable = { source = "log", name = "LSP Log", func = function() LspLogTerm:set_toggle(4) end },
})

XLogTerm = Terminal:new({
    on_open = function(term) Term_on_open(term); vim.b[0].my_term_title = "X Session Log" end,
    cmd = "tail --follow --retry ~/.xsession-errors | less -S",
    runnable = { source = "log", name = "X Session Log", func = function() LspLogTerm:set_toggle(4) end },
})

FocusTerm = Terminal:new({
    cmd = "focus",
})

LZGTerm = Terminal:new({
    cmd = "lazygit",
    direction = "float",
    on_open = function(term) Term_on_open(term); vim.b[0].my_term_title = "Lazy Git" end
})


Map("n", "<cr>n", function() _G.sendLines(vim.v.count, 1) end)
Map("n", "<cr>e", function() _G.sendLines(vim.v.count, 2) end)
Map("n", "<cr>i", function() _G.sendLines(vim.v.count, 3) end)
Map("n", "<cr>o", function() _G.sendLines(vim.v.count, 4) end)

Map("x", "<cr>n", ":<c-u>call v:lua.sendRegion(visualmode(), 1)<cr>", { remap = true })
Map("x", "<cr>e", ":<c-u>call v:lua.sendRegion(visualmode(), 2)<cr>", { remap = true })
Map("x", "<cr>i", ":<c-u>call v:lua.sendRegion(visualmode(), 3)<cr>", { remap = true })
Map("x", "<cr>o", ":<c-u>call v:lua.sendRegion(visualmode(), 4)<cr>", { remap = true })

Map("x", "<Plug>(1sendReg)", [[:<c-u>call v:lua.sendRegion(visualmode(), 1)<cr>]])
Map("x", "<Plug>(2sendReg)", [[:<c-u>call v:lua.sendRegion(visualmode(), 2)<cr>]])
Map("x", "<Plug>(3sendReg)", [[:<c-u>call v:lua.sendRegion(visualmode(), 3)<cr>]])
Map("x", "<Plug>(4sendReg)", [[:<c-u>call v:lua.sendRegion(visualmode(), 4)<cr>]])

local function harpsend(num)
    local to_send = vim.fn.getreg('"'):gsub("[\r\n]$", "")
    if num == 1 then
        Harp_Term_1:send_open(to_send, true, num)
    elseif num == 2 then
        Harp_Term_2:send_open(to_send, true, num)
    elseif num == 3 then
        Harp_Term_3:send_open(to_send, true, num)
    elseif num == 4 then
        Harp_Term_4:send_open(to_send, true, num)
    end
end

function _G.sendRange(startline, endline, num)
    local regStore = vim.fn.getreg('"')
    local regType = vim.fn.getregtype('"')
    vim.cmd(startline .. "," .. endline .. " yank")
    harpsend(num)
    vim.fn.setreg('"', regStore, regType)
end

function _G.sendLines(count, num)
    count = count + 1
    local regStore = vim.fn.getreg('"')
    local regType = vim.fn.getregtype('"')
    vim.cmd("normal! " .. count .. "yy")
    harpsend(num)
    vim.fn.setreg('"', regStore, regType)
end

-- function Terminal:sendOp()
--     local regStore = vim.fn.getreg('"')
--     local regType = vim.fn.getregtype('"')
--     if type == "line" then
--         vim.cmd([[normal! '[V']y]])
--     elseif type == "block" then
--         vim.cmd([[normal! `[\<C-v>`]\y]])
--     else
--         vim.cmd([[normal! `[v`]y]])
--     end
--     self:send(vim.fn.getreg('"'))
--     vim.fn.setreg('"', regStore, regType)
--     vim.cmd("normal! `z")
-- end

function _G.sendRegion(type, num)
    vim.cmd("normal <esc>")
    local regStore = vim.fn.getreg('"')
    local regType = vim.fn.getregtype('"')
    vim.cmd([[silent normal! `<]] .. type .. [[`>y]])
    harpsend(num)
    vim.fn.setreg('"', regStore, regType)
    vim.cmd("normal! `>")
end
