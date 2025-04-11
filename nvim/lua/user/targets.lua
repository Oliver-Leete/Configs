local M = {}

local dir_jumps = "search"

M.ai = function(direction, id)
    local count = vim.v.count
    dir_jumps = id
    vim.cmd("norm! m`")
    repeat
        MiniAi.move_cursor("left", "a", id, { search_method = direction })
        count = count - 1
    until count <= 0
end

M.func = function(func, key, args, opts)
    local count = vim.v.count
    dir_jumps = key
    vim.cmd("norm! m`")
    repeat
        func(args, opts)
        count = count - 1
    until count <= 0
end

M.plug_targets = function(count, movement, selection)
    local cmd = ""
    repeat
        cmd = cmd .. [[\<plug>]] .. movement
        count = count - 1
    until count <= 0

    cmd = cmd .. [[m\<plug>]] .. selection
    vim.cmd([[exe "normal ]] .. cmd .. [["]])
end

M.mapping = function(mapping, key)
    local count = vim.v.count
    dir_jumps = key
    vim.cmd("norm! m`")
    repeat
        vim.cmd("norm! " .. mapping)
        count = count - 1
    until count <= 0
end

M.command = function(command, key)
    local count = vim.v.count
    dir_jumps = key
    vim.cmd("norm! m`")
    repeat
        vim.cmd(command)
        count = count - 1
    until count <= 0
end

M.next = function()
    local return_map
    if dir_jumps == "search" then
        return_map = "n:norm! zz<cr>"
    elseif dir_jumps == "]" or dir_jumps == "[" then
        return_map = "]]"
    elseif dir_jumps == "}" or dir_jumps == "{" then
        return_map = "]}"
    elseif dir_jumps == ")" or dir_jumps == "(" then
        return_map = "])"
    else
        return_map = "]" .. dir_jumps
    end
    return vim.api.nvim_replace_termcodes(return_map, true, true, true)
end

M.prev = function()
    local return_map
    if dir_jumps == "search" then
        return_map = "N:norm! zz<cr>"
    elseif dir_jumps == "]" or dir_jumps == "[" then
        return_map = "[["
    elseif dir_jumps == "}" or dir_jumps == "{" then
        return_map = "[{"
    elseif dir_jumps == ")" or dir_jumps == "(" then
        return_map = "[("
    else
        return_map = "[" .. dir_jumps
    end
    return vim.api.nvim_replace_termcodes(return_map, true, true, true)
end

-- need to put this after targets
vim.keymap.set("x", "i", "<Plug>(niceblock-I)", { remap = true, nowait = true })
vim.keymap.set("x", "a", "<Plug>(niceblock-A)", { remap = true, nowait = true })

vim.keymap.set(
    { "n", "x", "o" },
    "<plug>(slash-after)",
    function() dir_jumps = "search" end,
    { remap = true }
)

vim.keymap.set(
    { "n", "x", "o" },
    "n",
    function() return M.next() end,
    { expr = true, remap = true, silent = true, desc = "Jump next" }
)
vim.keymap.set(
    { "n", "x", "o" },
    "N",
    function() return M.prev() end,
    { expr = true, remap = true, silent = true, desc = "Jump prev" }
)

return M
