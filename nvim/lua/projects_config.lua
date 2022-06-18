local handle = io.popen([[echo "$(basename "$PWD")"]])
local project
if handle then
    project = handle:read("*a")
    handle:close()
    vim.g.project = string.gsub(project, "\n", "")
else
    pcall(vim.notify("Couldn't find project name", "Warn", { title = "Julia" }))
end

-- JULIA

vim.g.projectionist_heuristics = {
    ["src/" .. vim.g.project .. ".jl"] = {
        ["src/*.jl"] = {
            type = "source",
            alternate = "test/{}_tests.jl",
            related = { "test/{}_tests.jl", "docs/src/{}.md" },
        },
        ["test/*_tests.jl"] = {
            type = "test",
            alternate = "src/{}.jl",
            related = { "src/{}.jl", "docs/src/{}.md" },
        },
        ["docs/src/*.md"] = {
            type = "doc",
            alternate = "src/{}.jl",
            related = { "test/{}_tests.jl", "src/{}.jl" },
        },

        ["src/" .. vim.g.project .. ".jl"] = {
            type = "mainSource",
            alternate = "test/" .. vim.g.project .. "Tests.jl",
            related = { "test/" .. vim.g.project .. "Tests.jl", "docs/make.jl" },
        },
        ["test/" .. vim.g.project .. "Tests.jl"] = {
            type = "mainTest",
            alternate = "src/" .. vim.g.project .. ".jl",
            related = { "src/" .. vim.g.project .. ".jl", "docs/make.jl" },
        },
        ["docs/make.jl"] = {
            type = "mainDoc",
            alternate = "src/}.jl",
            related = { "src/" .. vim.g.project .. ".jl", "test/" .. vim.g.project .. "Tests.jl" },
        },

        ["README.md"] = { type = "readme" },
        ["Project.toml"] = { type = "deps" },
        type = "julia",
    },
}

local juliaProjectRunnables = function()
    -- Misc Runnables
    local runnables_list = {
        {
            source = "Misc",
            name = "Open Runnable Terminal",
            command = [[silent !kittyPersistent JuliaPersistant juliaTest]],
        },
        {
            source = "Misc",
            name = "Precompile Package",
            command = [[silent !kittyOneShot "~/.config/nvim/filetype/julia/precompile"]],
        },
        {
            source = "Misc",
            name = "Build Documentation",
            command = [[silent !kittyOneShot "~/.config/nvim/filetype/julia/docBuild"]],
        },
        {
            source = "Misc",
            name = "Live Build Documentation",
            command = [[silent !kittyOneShot "julia --project=docs -ie 'using ]] ..
                vim.g.project .. [[, LiveServer; servedocs(launch_browser=true)'"]]
        },
        {
            source = "Misc",
            name = "Run Documentation Tests",
            command = [[silent !kittyOneShot "~/.config/nvim/filetype/julia/docTest"]],
        },
        {
            source = "Test",
            name = "Run All Tests",
            command = [[silent !kittyPersistent JuliaPersistant juliaTest ']]
                .. vim.g.project
                .. [[Tests.runtests(;spin=false)']],
        },
        {
            source = "Bench",
            name = "Run All Benchmarks",
            command = [[silent !kittyPersistent JuliaPersistant juliaTest 'run(]]
                .. vim.g.project
                .. [[Tests.suite, verbose=true)']],
        },
        {
            source = "Bench",
            name = "Retune Benchmarks",
            command = [[silent !kittyPersistent JuliaPersistant juliaTest 'let suite=]]
                .. vim.g.project
                ..
                [[Tests.suite; tune\!(suite); BenchmarkTools.save(joinpath(dirname(@__FILE__), "params.json"), params(suite));end']],
        },
    }

    -- Tests
    local handle1 = io.popen(
        [[rg --no-filename --no-heading --no-line-number -e "^\s*@testitem\s*\"(.*)\"\s*begin.*\$" -r "\$1"]]
    )
    local tests
    if handle1 then
        tests = handle1:read("*a")
        handle1:close()

        for name in tests:gmatch("([^\r\n]+)") do
            table.insert(runnables_list, {
                source = "Test",
                name = name,
                command = [[silent !kittyPersistent JuliaPersistant juliaTest ']]
                    .. vim.g.project
                    .. [[Tests.runtests("]]
                    .. name
                    .. [[",spin=false)']],
            })
        end
    end


    -- Benchmarks

    local handle2 = io.popen(
        [[rg --no-filename --no-heading --no-line-number -e ".*\[\"(.*?)\"\].*@benchmarkable(.*)\$" -r "\$1	\$2"]]
    )
    local benches
    if handle2 then
        benches = handle2:read("*a")
        handle2:close()

        for s in benches:gmatch("([^\r\n]+)") do
            local name, command = s:match("([^\t]+)\t([^\t]+)")
            table.insert(runnables_list, {
                source = "Bench",
                name = name,
                command = [[silent !kittyPersistent JuliaPersistant juliaTest 'run(]]
                    .. vim.g.project
                    .. [[Tests.suite["]]
                    .. name
                    .. [["], verbose=true)']],
            })
            table.insert(runnables_list, {
                source = "Prof",
                name = name,
                command = [[silent !kittyPersistent JuliaPersistant juliaTest 'a = @bprofile ]]
                    .. command
                    ..
                    [[; Profile.print(IOContext(open("/tmp/julprof.data", "w"), :displaysize=>(100000,1000)), format=:flat); ]]
                    .. [[ProfileView.view(); loadProfData(); a']],
            })
            table.insert(runnables_list, {
                source = "Debug",
                name = name,
                command = [[silent !kittyPersistent JuliaPersistant juliaTest '@run run(]]
                    .. vim.g.project
                    .. [[Tests.suite["]]
                    .. name
                    .. [["], verbose=true)']],
            })

        end
    end


    -- table.sort(runnables_list, function(a, b) return a.name < b.name end)
    -- Selection
    return runnables_list
end

-- RUST

vim.g.projectionist_heuristics = {
    ["src/*.rs"] = {
        ["src/*.rs"] = {
            type = "source",
            alternate = "tests/{}.rs",
            related = { "benches/{}.rs", "tests/{}.rs" },
        },
        ["benches/*.rs"] = {
            type = "bench",
            alternate = "src/{}.rs",
            related = { "src/{}.rs", "tests/{}.rs" },
        },
        ["tests/*.rs"] = {
            type = "test",
            alternate = "src/{}.rs",
            related = { "src/{}.rs", "benches/{}.rs" },
        },
        ["README.md"] = { type = "readme" },
        ["Cargo.toml"] = { type = "deps" },
        ["src/main.rs"] = { type = "mainSource" },
        ["tests/main.rs"] = { type = "mainTest" },
        ["benches/main.rs"] = { type = "mainBench" },
    },
}


function ActivateProject()
    if vim.fn.filereadable("src/" .. vim.g.project .. ".jl") ~= 0 then
        vim.g.runnables = juliaProjectRunnables
    else
    end
end

local projection = vim.api.nvim_create_augroup("projection", { clear = true })
vim.api.nvim_create_autocmd("VimEnter", { callback = ActivateProject, group = projection })
