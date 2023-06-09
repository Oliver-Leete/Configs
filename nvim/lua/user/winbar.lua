require("nvim-navic").setup({
    icons = {
        File = ' ',
        Module = ' ',
        Namespace = ' ',
        Package = ' ',
        Class = ' ',
        Method = ' ',
        Property = ' ',
        Field = ' ',
        Constructor = ' ',
        Enum = ' ',
        Interface = ' ',
        Function = ' ',
        Variable = ' ',
        Constant = ' ',
        String = ' ',
        Number = ' ',
        Boolean = ' ',
        Array = ' ',
        Object = ' ',
        Key = ' ',
        Null = ' ',
        EnumMember = ' ',
        Struct = ' ',
        Event = ' ',
        Operator = ' ',
        TypeParameter = ' '
    },
})
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
    local name = "NavicIcons" .. i
    Data = hl
    hl.background = Ct.diff.change
    vim.api.nvim_set_hl(0, name, hl)
    type_hl[i] = name
end
vim.api.nvim_set_hl(0, "NavicSeparator", { fg = Tc.fujiWhite, bg = Tc.winterBlue })
vim.api.nvim_set_hl(0, "NavicText", { fg = Tc.fujiWhite, bg = Tc.winterBlue })
