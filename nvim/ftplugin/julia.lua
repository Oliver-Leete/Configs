vim.bo.commentstring = [[#%s]]

vim.b[0].upafunc = function()
    local path = vim.fn.expand("%")
    if vim.startswith(path, "docs/") then
        vim.cmd.edit({ args = { "docs/make.jl" } })
    elseif vim.startswith(path, "test/") then
        vim.cmd.edit({ args = { "test/runtests.jl" } })
    elseif vim.startswith(path, "benchmark/") then
        vim.cmd.edit({ args = { "benchmark/benchmarks.jl" } })
    else
        vim.cmd.edit({ args = { "src/" .. vim.g.project .. ".jl" } })
    end
end
