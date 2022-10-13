Temp_Num = 50
Term_on_open = function(term)
    vim.wo[term.window].signcolumn = "no"
    if not term.jobname and term.id then
        term.jobname = "Terminal " .. term.id
    elseif not term.jobname and not term.id then
        Temp_Num = Temp_Num + 1
        term.jobname = "Terminal " .. Temp_Num
    end
    term.name = term.jobname
    vim.b[0].my_term_title = term.jobname
    vim.b[0].my_term_id = term.id
    vim.wo.winbar = "%{%v:lua.Term_Winbar()%}"
end

function Term_Winbar()
    local is_active = vim.api.nvim_get_current_win() == tonumber(vim.g.actual_curwin)
    local mode = VimMode()[2]
    if not mode then return "" end
    local hlb = "%#WinBarBlank#"
    local hl = is_active and "%#WinBar" .. mode .. "#" or "%#WinBarInactive#"
    local hle = is_active and "%#WinBar" .. mode .. "Ends#" or "%#WinBarInactiveEnds#"

    local term_id = vim.b[0].my_term_id
    local term_list = require("toggleterm.terminal").get_all(true)
    local cur_win = vim.api.nvim_get_current_win()
    local winbar = ""
    for _, term in pairs(term_list) do
        local is_cur_term = tonumber(term_id) == term.id
        if is_active and is_cur_term then
            hl = "%#WinBar" .. mode .. "#"
            hle = "%#WinBar" .. mode .. "Ends#"
        elseif is_cur_term then
            hl = "%#WinBarInactiveSpecial#"
            hle = "%#WinBarInactiveSpecialEnds#"
        else
            hl = "%#WinBarInactive#"
            hle = "%#WinBarInactiveEnds#"
        end
        winbar = winbar .. string.format("%%%d@v:lua.my_toggleterm_winbar_click@", term.id + cur_win * 10000000)
        winbar = winbar .. hle .. ""
        winbar = winbar .. hl .. " " .. " " .. term.jobname .. " "
        winbar = winbar .. hle .. ""
    end
    return hlb .. winbar .. hle .. "%="
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
    shell = "fish",
    float_opts = {
        border = Border,
    }
})

function AnyTermOpen()
    local term_list = require("toggleterm.terminal").get_all()
    local open
    for _, term in pairs(term_list) do
        if term:is_open() then
            open = true
            break
        end
    end
    return open
end

Terminal = require("toggleterm.terminal").Terminal
Harp_Term_1 = Terminal:new({ id = 1001, jobname = "Terminal 1" })
Harp_Term_2 = Terminal:new({ id = 1002, jobname = "Terminal 2" })

function Terminal:open_no_foc()
    if not self:is_open() then
        self:toggle()
    end
    local ui = require("toggleterm.ui")
    ui.scroll_to_bottom()
    ui.goto_previous()
    ui.stopinsert()
end

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
            Harp_Term_2 = self
        end
    end
end

function Terminal:set_toggle(term_num)
    self:set_harp(term_num)
    self:toggle()
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

local function harpsend(num)
    local to_send = vim.fn.getreg('"'):gsub("[\r\n]$", "")
    if num == 1 then
        Harp_Term_1:send_open(to_send, true, num)
    elseif num == 2 then
        Harp_Term_2:send_open(to_send, true, num)
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

function _G.sendRegion(type, num)
    vim.cmd("normal <esc>")
    local regStore = vim.fn.getreg('"')
    local regType = vim.fn.getregtype('"')
    vim.cmd([[silent normal! `<]] .. type .. [[`>y]])
    harpsend(num)
    vim.fn.setreg('"', regStore, regType)
    vim.cmd("normal! `>")
end
