require("nvim-navic").setup({})
local func = require("user.myfuncs")

local left = ""
local right = ""

local type_hl = {
    File = "Directory",
    Module = "@include",
    Namespace = "@namespace",
    Package = "@include",
    Class = "@structure",
    Method = "@method",
    Property = "@property",
    Field = "@field",
    Constructor = "@constructor",
    Enum = "@field",
    Interface = "@type",
    Function = "@function",
    Variable = "@variable",
    Constant = "@constant",
    String = "@string",
    Number = "@number",
    Boolean = "@boolean",
    Array = "@field",
    Object = "@type",
    Key = "@keyword",
    Null = "@comment",
    EnumMember = "@field",
    Struct = "@struct",
    Event = "@keyword",
    Operator = "@operator",
    TypeParameter = "@type",
}
for i, id in pairs(type_hl) do
    local is_hl_set = function(hl_name)
        local exists, hl = pcall(vim.api.nvim_get_hl_by_name, hl_name, true)
        local color = hl.foreground or hl.background or hl.reverse
        return exists and color ~= nil
    end

    local hl
    if is_hl_set(id) then
        hl = vim.api.nvim_get_hl_by_name(id, true)
    else
        hl = vim.api.nvim_get_hl_by_name("Normal", true)
    end
    local name = "navic" .. id
    Data = hl
    hl.background = Ct.diff.change
    vim.api.nvim_set_hl(0, name, hl)
    type_hl[i] = name
end
vim.api.nvim_set_hl(0, "NavicSep", { fg = Tc.fujiWhite, bg = Tc.winterBlue })
vim.api.nvim_set_hl(0, "NavicEnd", { fg = Ct.diff.change, bg = Ct.ui.bg })

Navic_on_click = function(pos)
    local line = math.floor(pos / 100000)
    local col = pos - (line * 100000)
    vim.api.nvim_win_set_cursor(vim.fn.win_getid(0), { line, col })
end

local navic_maker = function()
    local data = require("nvim-navic").get_data() or {}
    local ret = " "
    for i, d in ipairs(data) do
        local pos = d.scope.start.line * 100000 + d.scope.start.character
        if i > 1 then ret = ret .. "  " end
        ret = ret ..
            "%#" .. type_hl[d.type] .. "#" ..
            string.format("%%%d@v:lua.Navic_on_click@", pos) ..
            d.icon ..
            d.name:gsub("%%", "%%%%"):gsub("%s*->%s*", ''):gsub(":.*", "") .. "%#NavicSep#" ..
            "%X"
    end
    return ret
end

local overseer = require("overseer")
local special_name = function(bufnr)
    if vim.bo[bufnr].filetype == "help" then
        local help_title = vim.fn.expand("%:t:r")
        help_title = help_title:gsub("(%l)(%w*)", function(a, b) return string.upper(a) .. b end)
        return help_title .. " Help", false
    elseif vim.bo[bufnr].buftype == "terminal" then
        local task = vim.tbl_filter(
            function(t)
                if t.strategy.bufnr == bufnr then
                    return true
                else
                    return false
                end
            end,
            overseer.list_tasks()
        )[1]
        if task then return task.name, true end
        return "Terminal", true
    else
        local filetype = func.special_types[vim.bo[bufnr].filetype].name
        if filetype then
            return filetype, false
        end
    end
end

Special_Winbar = function()
    local bufnr = vim.api.nvim_get_current_buf()
    local is_active = vim.api.nvim_get_current_win() == tonumber(vim.g.actual_curwin)

    local mode = func.vim_mode()[2]
    if not mode then return "" end

    local hlb = "%#WinBarBlank#"
    local hl = is_active and "%#WinBar" .. mode .. "#" or "%#WinBarInactive#"
    local hle = is_active and "%#WinBar" .. mode .. "Ends#" or "%#WinBarInactiveEnds#"

    local specialname, is_term = special_name(bufnr)
    if not specialname then return "" end
    if is_term and is_active and mode == "Terminal" then
        return hl .. "%= " .. specialname .. " %="
    else
        return hlb .. "%=" .. hle .. "" .. hl .. " " .. specialname .. " " .. hle .. "" .. hlb .. "%=" .. hlb
    end
end

-- File_on_click = function(bufnr)
--     vim.cmd("Neotree " .. vim.api.nvim_buf_get_name(bufnr))
-- end

Normal_Winbar = function()
    local bufnr = vim.api.nvim_get_current_buf()
    local is_active = vim.api.nvim_get_current_win() == tonumber(vim.g.actual_curwin)

    local mode = func.vim_mode()[2]
    if not mode then return "" end

    local hlb = "%#WinBarBlank#"
    local hl = is_active and "%#WinBar" .. mode .. "#" or "%#WinBarInactive#"
    local hle = is_active and "%#WinBar" .. mode .. "Ends#" or "%#WinBarInactiveEnds#"

    local winbar = hl
    -- Ismodified icon
    if vim.bo.modified or vim.bo.modifiable == false then
        winbar = winbar .. " "
    end
    -- File icon
    if vim.fn.expand("%") ~= "" then
        winbar = winbar .. string.format("%%%d@v:lua.File_on_click@", bufnr)
        winbar = hl .. " " .. winbar .. vim.fn.pathshorten(vim.fn.expand('%:p:~:.'), 3)
    end
    -- Code location
    winbar = winbar .. "%X"
    if is_active and require("nvim-navic").is_available() then
        winbar = winbar .. " " .. "%#WinBar" .. mode .. "NavicEnds#" .. right
        winbar = winbar .. navic_maker() .. " " .. "%#NavicEnd#" .. right
    else
        winbar = winbar .. " " .. hle .. right
    end
    winbar = winbar .. hlb .. "%="
    -- File icon
    if vim.fn.expand("%") ~= "" then
        local icon, iconhl = require("nvim-web-devicons").get_icon(vim.fn.expand("%:t"), vim.fn.expand("%:e"))
        if icon == "" or icon == nil then
            icon = ""
        end
        if iconhl == "" or iconhl == nil then
            iconhl = "Normal"
        end
        winbar = winbar .. "%#" .. iconhl .. "#" .. icon .. " "
    end
    return hle .. " " .. winbar
end
