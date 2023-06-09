local save_wordcount = function()
    local time = os.date("%Y-%m-%dT%X")
    local wordcount_output = vim.api.nvim_command_output("VimtexCountWord")
    if wordcount_output then
        local wordcount = wordcount_output:match(".-: (%d+).-")
        if not wordcount then
            wordcount = wordcount_output:match("(%d%d%d+).-$")
        end
        local file = io.open("/home/oleete/Documents/University/wordcount.csv", "a")
        if file then
            file.write(file, time .. "," .. wordcount .. "\n")
        end

    end
end

if vim.g.countSaved ~= 1 then
    save_wordcount()
    vim.fn.jobstart([[julia -e 'using CSV, DataFrames, Plots; a = CSV.File("/home/oleete/Documents/University/wordcount.csv") |> DataFrame; p = plot(a[!,1], a[!,2], legend=false); savefig(p, "/home/oleete/Documents/University/wordcount.png")']])
    vim.g.countSaved = 1
end

if vim.g.viewerOpen ~= 1 then
    vim.g.viewerOpen = 1
    vim.cmd("VimtexView")
    vim.cmd("sleep 200m")
    vim.cmd("silent !xdotool key super+n")
end
