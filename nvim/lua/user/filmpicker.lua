local M = {}
local func = require("user.myfuncs")
-- Settings for filmpicker script
M.runtime = function()
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
    return string.format("Duration: %02d:%02d:%02d", hours, minutes, seconds)
end

M.endtime = function()
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
    local format_string
    if runtime >= 86400 then
        format_string = "Endtime: %x %X"
    else
        format_string = "Endtime: %X"
    end
    return os.date(format_string, os.time() + runtime)
end

local filmPicker = vim.api.nvim_create_augroup("filmPicker", { clear = true })
vim.api.nvim_create_autocmd("BufRead", { pattern = "/tmp/film_list.films", callback = function()
    vim.keymap.set("n", "<leader>a", "vip:!sort -k1<cr><cr>")
    vim.keymap.set("n", "<leader>r", "vip:!sort -k3 -h<cr><cr>")
    vim.keymap.set("n", "<leader>s", "vip:!sort -k5<cr><cr>")
end, group = filmPicker })

return M
