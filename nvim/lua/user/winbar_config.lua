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
        winbar = hl .. " " .. winbar .. vim.api.nvim_buf_get_name(bufnr) .. " "
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

function Normal_Winbar()
    local bufnr = vim.api.nvim_get_current_buf()
    local is_active = vim.api.nvim_get_current_win() == tonumber(vim.g.actual_curwin)

    local mode = VimMode()[2]
    if not mode then return "" end

    local hlb = "%#WinBarBlank#"
    local hl = is_active and "%#WinBar" .. mode .. "#" or "%#WinBarInactive#"
    local hle = is_active and "%#WinBar" .. mode .. "Ends#" or "%#WinBarInactiveEnds#"

    local winbar = ""
    if Is_special(bufnr) then
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

vim.go.winbar = "%{%v:lua.Normal_Winbar()%}"
