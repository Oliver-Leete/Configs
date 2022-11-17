local save_wordcount = function()
    local time = os.date("%Y-%m-%dT%X")
    local wordcount_output = vim.api.nvim_command_output("VimtexCountWord")
    if wordcount_output then
        local wordcount = wordcount_output:match(".-: (%d+).-")
        local file = io.open("/home/oleete/Documents/University/wordcount.csv", "a")
        if file then
            file.write(file, time .. "," .. wordcount .. "\n")
        end
    end
end

if vim.g.countSaved ~= 1 then
    save_wordcount()
    vim.g.countSaved = 1
end
