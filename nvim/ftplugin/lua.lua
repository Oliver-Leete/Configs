vim.b[0].localCommands = function()
    return {
        { source = "lua", name = "Source file", command = "source %" }
    }
end

local ts_input = require('mini.surround').gen_spec.input.treesitter
vim.b.minisurround_config = {
    custom_surroundings = {
        a = {
            input = { "function%(.-%).-end", "^function%(%)%s?().-()%s?end$" },
            output = { left = 'function() ', right = ' end' },
        },
    }
}
