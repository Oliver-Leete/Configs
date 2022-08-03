vim.b[0].localCommands = function()
    return {
        { source = "lua", name = "Source file", command = "source %" }
    }
end

vim.b.minisurround_config = {
    custom_surroundings = {
        ['a'] = {
            input = { find = "function%(.-%).-end", extract = "^(function%(%)%s?).-(%s?end)$" },
            output = { left = 'function() ', right = ' end' },
        },
        ["s"] = {
            input = { find = '%[%[.-%]%]', extract = '^(..).*(..)$' },
            output = { left = '[[', right = ']]' },
        },
    }
}
