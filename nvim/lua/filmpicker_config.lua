-- Settings for filmpicker script
function Filmpicker_winbar()
    local mode = VimMode()[2]
    local hlb = "%#WinBarBlank#"
    local hl = "%#WinBar" .. mode .. "#"
    local hle = "%#WinBar" .. mode .. "Ends#"
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
    local winbar = hl .. string.format("%02d:%02d:%02d", hours, minutes, seconds)
    return hlb .. hle .. "" .. winbar .. hl .. " " .. hle .. "" .. hlb .. "%=" .. hlb
end

local filmPicker = vim.api.nvim_create_augroup("filmPicker", { clear = true })
vim.api.nvim_create_autocmd("BufRead", { pattern = "*.films", callback = function()
    Map("n", "<leader>a", "vip:!sort -k1<cr><cr>")
    Map("n", "<leader>r", "vip:!sort -k3<cr><cr>")
    Map("n", "<leader>s", "vip:!sort -k5<cr><cr>")

    vim.go.winbar = "%{%v:lua.Filmpicker_winbar()%}"
end, group = filmPicker })
