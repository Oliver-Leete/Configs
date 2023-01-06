require("nvim-navic").setup({})

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
    local function is_hl_set(hl_name)

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
    hl.background = Tc.winterBlue
    vim.api.nvim_set_hl(0, name, hl)
    type_hl[i] = name
end
vim.api.nvim_set_hl(0, "NavicSep", { fg = Tc.fujiWhite, bg = Tc.winterBlue })
vim.api.nvim_set_hl(0, "NavicEnd", { fg = Tc.winterBlue, bg = Tc.sumiInk1 })

Navic_on_click = function(pos)
    local line = math.floor(pos / 100000)
    local col = pos - (line * 100000)
    vim.api.nvim_win_set_cursor(vim.fn.win_getid(0), { line, col })
end

local navic_maker = function(hl)
    local data = require("nvim-navic").get_data() or {}
    local ret = ""
    for i, d in ipairs(data) do
        local pos = d.scope.start.line * 100000 + d.scope.start.character
        if i > 1 then ret = ret .. " > " end
        ret = ret ..
            "%#" .. type_hl[d.type] .. "#" ..
            string.format("%%%d@v:lua.Navic_on_click@", pos) ..
            d.icon ..
            d.name:gsub("%%", "%%%%"):gsub("%s*->%s*", ''):gsub(":.*", "") .. "%#NavicSep#" ..
            "%X"
    end
    return ret
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

local function special_winbar(bufnr, hl)
    local mode = VimMode()[2]
    local specialname = SpecialName(bufnr)
    if not specialname then return "" end
    return hl .. " " .. specialname .. " "
end

-- local panel_types = {
--     "OverseerList",
--     "neo-tree",
--     "neotest-summary"
-- }
--
-- Panel_on_click = function(id)
--     old = math.floor(id / 1000)
--     new = id - (old * 1000)
--     vim.api.nvim_win_close(old)
--     Special_types[panel_types[new]].open()
-- end
--
-- local function panel_winbar(type, bufnr)
--     local winbar = ""
--     -- local pos = panel_types[vim.bo[bufnr].filetype] * 1000
--     return winbar
-- end

function File_on_click(bufnr)
    vim.cmd("Neotree " .. vim.api.nvim_buf_get_name(bufnr))
end

local function default_winbar(bufnr, hl, is_active, hle)
    local winbar = hl
    local hlb = "%#WinBarBlank#"
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
    if is_active and require("nvim-navic").is_available() then
        local mode = VimMode()[2]
        winbar = winbar .. " " .. "%#WinBar" .. mode .. "NavicEnds#" .. " "
        winbar = winbar .. navic_maker(hl) .. " " .. "%#NavicEnd#" .. ""
    else
        winbar = winbar .. " " .. hle .. ""
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
        -- local type = Special_types[vim.bo[bufnr].filetype]
        -- if type.on_panel then
        --     winbar = panel_winbar(type, bufnr)
        --     return "test"
        --     -- return winbar .. hlb .. "%="
        -- end
        winbar = special_winbar(bufnr, hl)
        if winbar == "" then return "" end
        return hlb .. "%=" .. hle .. "" .. winbar .. hle .. "" .. hlb .. "%=" .. hlb
    else -- Default winbar
        winbar = default_winbar(bufnr, hl, is_active, hle)
    end

    if winbar == "" then return "" end
    return winbar
end

vim.go.winbar = "%{%v:lua.Normal_Winbar()%}"
