local function filmpicker_winbar(hl)
    local line1 = vim.fn.search([[\(\%^\|^$\)]], "nbWc") - 1
    local line2 = vim.fn.search([[\(\%$\|^$\)]], "nW")

    local lines = vim.api.nvim_buf_get_lines(0, line1, line2, false)
    local pattern = "(%d+):(%d+):(%d+)"
    local runtime = 0
    for _, line in pairs(lines) do
        local time_string = line:sub(1, 8)
        local hour, minute, second = time_string:match(pattern)
        if hour and minute and second then
            runtime = runtime + hour * 3600 + minute * 60 + second
        end
    end
    local hours = math.floor(runtime / 3600)
    local minutes = math.floor(math.fmod(runtime, 3600) / 60)
    local seconds = math.floor(math.fmod(runtime, 60))
    return hl .. string.format("%02d:%02d:%02d", hours, minutes, seconds)
end

SpecialName = function(bufnr)
    if vim.bo[bufnr].filetype == "help" then
        local help_title = vim.fn.expand("%:t:r")
        help_title = help_title:gsub("(%l)(%w*)", function(a, b) return string.upper(a) .. b end)
        return help_title .. " Help"
    else
        local filetype = Special_types[vim.bo[bufnr].filetype].name
        if filetype then
            return filetype
        end
    end
end

local function special_winbar(bufnr, hl, is_active)
    local mode = VimMode()[2]
    local specialname = SpecialName(bufnr)
    if not specialname then return "" end
    if is_active then
        hl = "%#WinBar" .. mode .. "#"
    else
        hl = "%#WinBarInactiveSpecial#"
    end
    return hl .. " " .. specialname .. " "
end

local function default_winbar(bufnr, hl, is_active)
    local winbar = hl
    -- Ismodified icon
    if vim.bo.modified or vim.bo.modifiable == false then
        winbar = winbar .. " "
    end
    -- File icon
    if vim.fn.expand("%") ~= "" then
        local icon = require("nvim-web-devicons").get_icon(vim.fn.expand("%:t"), vim.fn.expand("%:e"))
        if icon == "" or icon == nil then
            icon = ""
        end
        winbar = winbar .. icon .. " "
        winbar = hl .. " " .. winbar .. Get_unique_bufname(bufnr) .. " "
    end
    -- Code location
    if is_active and require("nvim-navic").is_available() then
        local location = require("nvim-navic").get_location()
        if location ~= "" then
            winbar = winbar .. "  " .. location
        end
    end
    return winbar
end

function _G.my_toggleterm_winbar_click(id)
    local cur_win = math.floor(id / 10000000)
    print(cur_win)
    local term_id = id - cur_win * 10000000
    print(term_id)
    local term = require("toggleterm.terminal").get_or_create_term(term_id)
    if not term then return end
    vim.api.nvim_win_set_buf(cur_win, term.bufnr)
end

function GPS_Bar()
    local bufnr = vim.api.nvim_get_current_buf()
    local is_active = vim.api.nvim_get_current_win() == tonumber(vim.g.actual_curwin)

    local mode = VimMode()[2]
    if not mode then return "" end

    local hlb = "%#WinBarBlank#"
    local hl = is_active and "%#WinBar" .. mode .. "#" or "%#WinBarInactive#"
    local hle = is_active and "%#WinBar" .. mode .. "Ends#" or "%#WinBarInactiveEnds#"

    local winbar = ""
    -- Special winbar for filmpicker script
    if vim.api.nvim_buf_get_name(bufnr) == "/tmp/film_list.films" then
        winbar = filmpicker_winbar(hl)
    elseif vim.bo[bufnr].filetype == "toggleterm" then
        local term_id = vim.b[0].my_term_id
        local term_list = require("toggleterm.terminal").get_all(true)
        local cur_win = vim.api.nvim_get_current_win()
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
            winbar = winbar .. hl .. ""
            winbar = winbar .. hl .. " " .. " " .. term.jobname .. " "
            winbar = winbar .. hl .. ""
        end
        return hlb .. winbar .. hle .. "%="
    elseif Is_special(bufnr) then
        winbar = special_winbar(bufnr, hl, is_active)
        hle = is_active and "%#WinBar" .. mode .. "Ends#" or "%#WinBarInactiveSpecialEnds#"
        if winbar == "" then return "" end
        return hlb .. "%=" .. hle .. "" .. winbar .. hle .. "" .. hlb .. "%=" .. hlb
    else -- Default winbar
        winbar = default_winbar(bufnr, hl, is_active)
    end

    if winbar == "" then return "" end
    return hlb .. hle .. "" .. winbar .. hl .. " " .. hle .. "" .. hlb .. "%=" .. hlb
end

vim.go.winbar = "%{%v:lua.GPS_Bar()%}"
