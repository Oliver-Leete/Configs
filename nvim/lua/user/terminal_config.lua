Temp_Num = 50
Term_on_open = function(term)
    vim.b[0].my_term_id = term.id
    vim.wo.winbar = "%{%v:lua.Term_Winbar()%}"
    if not term.hidden then
        OTerm = term
    end
end

function _G.my_toggleterm_winbar_click(id)
    local cur_win = math.floor(id / 100000)
    local term_id = id - (cur_win * 100000)
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
    local term_list = require("toggleterm.terminal").get_all()
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
        winbar = winbar .. hl .. " " .. " " .. term.name .. " "
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
