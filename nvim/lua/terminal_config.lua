Temp_Num = 50
Term_on_open = function(term)
    vim.wo[term.window].signcolumn = "no"
    if not term.jobname and term.id then
        term.jobname = "Terminal " .. term.id
    elseif not term.jobname and not term.id then
        Temp_Num = Temp_Num + 1
        term.jobname = "Terminal " .. Temp_Num
    end
    vim.b[0].my_term_title = term.jobname
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
    persist_mode = true,
    insert_mappings = true,
    terminal_mappings = true,
    persist_size = true,
    direction = "horizontal",
    close_on_exit = false,
    on_exit = function(t, _, exit_code, _)
        if exit_code == 0 then
            vim.notify(t.jobname .. " Succeded", "info", { title = "Terminal" })
        else
            vim.notify(t.jobname .. " has errored", "info", { title = "Terminal" })
            if not t:is_open() then
                t:open()
            end
        end
    end,
    shell = "fish",
    float_opts = {
        border = "curved",
    }
})

Terminal = require("toggleterm.terminal").Terminal
BackgroundTerm = Terminal:new({
    id = 4,
    runnable = { source = "def", name = "Background Terminal", func = function() BackgroundTerm:set_toggle(4) end }
})

Harp_Term_1 = Terminal:new({ id = 1 })
Harp_Term_2 = Terminal:new({ id = 2 })
Harp_Term_3 = Terminal:new({ id = 3 })
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

function Terminal:set_background()
    self:set_harp(2)
    self:shutdown()
    self:open()
    self:close()
end

function Terminal:send_open(cmd, _, term_num)
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
    cmd = "tail --follow --retry ~/.local/state/nvim/log | less -S",
    runnable = { source = "log", name = "Neovim Log", func = function() NvimLogTerm:set_toggle(4) end },
    jobname = "Neovim Log",
    id = 5,
})

LspLogTerm = Terminal:new({
    cmd = "tail --follow --retry ~/.local/state/nvim/lsp.log | less -S",
    runnable = { source = "log", name = "LSP Log", func = function() LspLogTerm:set_toggle(4) end },
    jobname = "LSP Log",
    id = 6,
})

XLogTerm = Terminal:new({
    cmd = "tail --follow --retry ~/.xsession-errors | less -S",
    runnable = { source = "log", name = "X Session Log", func = function() LspLogTerm:set_toggle(4) end },
    jobname = "X Session Log",
    id = 6,
})

FocusTerm = Terminal:new({
    cmd = "focus",
    jobname = "Focus",
    id = 6,
})

LZGTerm = Terminal:new({
    cmd = "lazygit",
    jobname = "Lazygit",
    id = 6,
})


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
