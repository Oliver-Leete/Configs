vim.b[0].runnables = function()
    return {
        { source = "lua", name = "Source file", command = "source %"}
    }
end

require("nvim-surround").buffer_setup({
    delimiters = {
        pairs = {
            ["a"] = { "function() ", " end" },
        }
    }
})
