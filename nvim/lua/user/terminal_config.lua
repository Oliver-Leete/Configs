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

function _G.my_toggleterm_winbar_click(id)
    local cur_win = math.floor(id / 100000)
    print(cur_win)
    local term_id = id - (cur_win * 100000)
    print(term_id)
    local term = require("toggleterm.terminal").get_or_create_term(term_id)
    if not term then return end
    vim.api.nvim_win_set_buf(cur_win, term.bufnr)
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
        winbar = winbar .. string.format("%%%d@v:lua.my_toggleterm_winbar_click@", term.id + cur_win * 100000)
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

Terminal = require("toggleterm.terminal").Terminal
Map("n", "<leader>I", function() _G.sendLines(vim.v.count) end)
Map("x", "<leader>I", ":<c-u>call v:lua.sendRegion(visualmode())<cr>")

function Terminal:fsend()
    local to_send = vim.fn.getreg('"'):gsub("[\r\n]$", "")
    self:send(to_send, true)
end

function _G.sendRange(startline, endline)
    local regStore = vim.fn.getreg('"')
    local regType = vim.fn.getregtype('"')
    vim.cmd(startline .. "," .. endline .. " yank")
    if STerm == "" then
        vim.notify("No terminal found", "info", { title = "STerm", })
    else
        STerm:fsend()
    end
    vim.fn.setreg('"', regStore, regType)
end

function _G.sendLines(count)
    count = count + 1
    local regStore = vim.fn.getreg('"')
    local regType = vim.fn.getregtype('"')
    vim.cmd("normal! " .. count .. "yy")
    if STerm == "" then
        vim.notify("No terminal found", "info", { title = "STerm", })
    else
        STerm:fsend()
    end
    vim.fn.setreg('"', regStore, regType)
end

function _G.sendRegion(type)
    vim.cmd("normal <esc>")
    local regStore = vim.fn.getreg('"')
    local regType = vim.fn.getregtype('"')
    vim.cmd([[silent normal! `<]] .. type .. [[`>y]])
    if STerm == "" then
        vim.notify("No terminal found", "info", { title = "STerm", })
    else
        STerm:fsend()
    end
    vim.fn.setreg('"', regStore, regType)
    vim.cmd("normal! `>")
end
