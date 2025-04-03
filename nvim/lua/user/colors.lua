local M = {}

M.colors = function(props)
    return props.focused
        and { fg = "#1F1F28", bg = "#7E9CD8", }
        or { fg = "#1F1F28", bg = "#4A4A60", }
end

return M
