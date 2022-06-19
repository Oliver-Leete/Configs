require("toggleterm").setup({
    size = function()
        local height = vim.api.nvim_list_uis()[1].height
        return math.floor(height * 0.3)
    end,
    shade_terminals = false,
    hide_numbers = true,
    start_in_insert = false,
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
    on_open = function() vim.b[0].my_term_title = "Background" end,
    runnable = { source = "Julia", name = "Julia Doc Server", func = function() JuliaLiveDocs:set_toggle(4) end }
})

Harp_Term_1 = Terminal:new()
Harp_Term_2 = Terminal:new({ on_open = function() vim.b[0].my_term_title = "One Shots" end })
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
        self:open()
    end
    self:send(cmd, go_back)
end

Background_Term_List = {
    { source = "default", name = "default", func = function() BackgroundTerm:set_toggle(4) end },
}

function Terminal:open_add()
    table.insert(Background_Term_List, self.runnable)
    self:set_harp(4)
    self:open()
end

JuliaTest = Terminal:new({
    cmd = "juliaTest",
    on_open = function() vim.b[0].my_term_title = "Julia Test" end
})

JuliaREPL = Terminal:new({
    cmd = "julia",
    on_open = function() vim.b[0].my_term_title = "Julia REPL" end
})

FocusTerm = Terminal:new({
    cmd = "focus",
})

LZGTerm = Terminal:new({
    cmd = "lazygit",
    on_open = function() vim.b[0].my_term_title = "Lazy Git" end
})


Map("n", "<cr>n", function() Harp_Term_1:sendLines(vim.v.count) end)
Map("n", "<cr>e", function() Harp_Term_2:sendLines(vim.v.count) end)
Map("n", "<cr>i", function() Harp_Term_3:sendLines(vim.v.count) end)
Map("n", "<cr>o", function() Harp_Term_4:sendLines(vim.v.count) end)

Map("x", "<cr>n", "<Plug>(1sendReg)", { remap = true })
Map("x", "<cr>e", "<Plug>(2sendReg)", { remap = true })
Map("x", "<cr>i", "<Plug>(3sendReg)", { remap = true })
Map("x", "<cr>o", "<Plug>(4sendReg)", { remap = true })

Map("x", "<Plug>(1sendReg)", [[:<c-u>call v:lua.sendRegion(visualmode(), 1)<cr>]])
Map("x", "<Plug>(2sendReg)", [[:<c-u>call v:lua.sendRegion(visualmode(), 2)<cr>]])
Map("x", "<Plug>(3sendReg)", [[:<c-u>call v:lua.sendRegion(visualmode(), 3)<cr>]])
Map("x", "<Plug>(4sendReg)", [[:<c-u>call v:lua.sendRegion(visualmode(), 4)<cr>]])

function Terminal:sendRange(startline, endline)
    local regStore = vim.fn.getreg('"')
    local regType = vim.fn.getregtype('"')
    vim.cmd(startline .. "," .. endline .. " yank")
    self:send(vim.fn.getreg('"'))
    vim.fn.setreg('"', regStore, regType)
end

function Terminal:sendLines(count)
    count = count + 1
    local regStore = vim.fn.getreg('"')
    local regType = vim.fn.getregtype('"')
    vim.cmd("normal! " .. count .. "yy")
    self:send(vim.fn.getreg('"'))
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
    local regStore = vim.fn.getreg('"')
    local regType = vim.fn.getregtype('"')
    vim.cmd([[silent normal! `<]] .. type .. [[`>y]])
    if num == 1 then
        Harp_Term_1:send(vim.fn.getreg('"'))
    elseif num == 2 then
        Harp_Term_2:send(vim.fn.getreg('"'))
    elseif num == 3 then
        Harp_Term_3:send(vim.fn.getreg('"'))
    elseif num == 4 then
        Harp_Term_4:send(vim.fn.getreg('"'))
    end
    vim.fn.setreg('"', regStore, regType)
    vim.cmd("normal! `>")
end
