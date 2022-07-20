local handle = io.popen([[echo "$(basename "$PWD")"]])
local project
if handle then
    project = handle:read("*a")
    handle:close()
    vim.g.project = string.gsub(project, "\n", "")
else
    vim.notify("Couldn't find project name", "Warn", { title = "Projects" })
end

-- JULIA

local juliaProjectRunnables = function()
    -- Misc Runnables
    local runnables_list = {
        {
            source = "Misc",
            name = "Open REPL",
            func = function() JuliaREPL:set_toggle(3) end,
        },
        {
            source = "Misc",
            name = "Open Test Terminal",
            func = function() JuliaTest:set_toggle(1) end,
        },
        {
            source = "Misc",
            name = "Precompile Package",
            func = function() JuliaPrecompile:set_background() end,
        },
        {
            source = "Misc",
            name = "Build Documentation",
            func = function() JuliaBuildDocs:set_background() end,
        },
        {
            source = "Misc",
            name = "Start Documentation Server",
            func = function() JuliaLiveDocs:open_add() end,
        },
        {
            source = "Misc",
            name = "Open Built Documentation",
            func = function() JuliaOpenDocs:set_background() end,
        },
        {
            source = "Misc",
            name = "Open Documentation Server",
            func = function() JuliaOpenDocServ:set_background() end,
        },
        {
            source = "Misc",
            name = "Run Documentation Tests",
            func = function() JuliaDocTests:set_background() end,
        },
        {
            source = "Test",
            name = "Run All Tests",
            func = function() JuliaTest:send_open(vim.g.project .. [[Tests.runtests(;spin=false)]], true, 1) end,
        },
        {
            source = "Bench",
            name = "Run All Benchmarks",
            func = function() JuliaTest:send_open("run(" .. vim.g.project .. [[Tests.suite, verbose=true)]], true, 1) end,
        },
        {
            source = "Bench",
            name = "Retune Benchmarks",
            func = function() JuliaTest:send_open("let suite=" ..
                    vim.g.project ..
                    [[Tests.suite; tune!(suite); BenchmarkTools.save(joinpath(dirname(@__FILE__), "params.json"), params(suite));end]]
                    , true, 1)
            end,
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
                func = function()
                    JuliaTest:send_open(vim.g.project .. [[Tests.runtests("]] .. name .. [[",spin=false)]], true, 1)
                end,
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
                func = function()
                    JuliaTest:send_open("run(" .. vim.g.project .. [[Tests.suite["]] .. name .. [["], verbose=true)]],
                        true, 1)
                end,
            })
            table.insert(runnables_list, {
                source = "Prof",
                name = name,
                func = function()
                    JuliaTest:send_open("a = @bprofile " .. command
                        ..
                        [[; Profile.print(IOContext(open("/tmp/julprof.data", "w"), :displaysize=>(100000,1000)), format=:flat); ]]
                        .. [[ProfileView.view(); loadProfData(); a]], true, 1)
                end,
            })
            table.insert(runnables_list, {
                source = "Debug",
                name = name,
                func = function()
                    JuliaTest:send_open("@run run(" ..
                        vim.g.project .. [[Tests.suite["]] .. name .. [["], verbose=true)]],
                        true, 1)
                end,
            })

        end
    end

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
    ["src/*.jl"] = {
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


function ActivateProject()
    if vim.fn.filereadable("src/" .. vim.g.project .. ".jl") ~= 0 then
        vim.g.runnables = juliaProjectRunnables

        JuliaTest = Terminal:new({
            jobname = "Julia Test",
            cmd = "juliaTest",
        })

        JuliaREPL = Terminal:new({
            jobname = "Julia REPL",
            cmd = "julia",
        })

        JuliaLiveDocs = Terminal:new({
            jobname = "Julia Doc Server",
            cmd = [[julia --project=docs -ie 'using ]] ..
                vim.g.project .. [[, LiveServer; servedocs(launch_browser=true)']],
            runnable = { source = "Julia", name = "Julia Doc Server", func = function() JuliaLiveDocs:set_toggle(4) end }
        })

        JuliaPrecompile = Terminal:new({
            jobname = "Package Precompile",
            cmd = "~/.config/nvim/filetype/julia/precompile",
        })

        JuliaBuildDocs = Terminal:new({
            jobname = "Build Documentation",
            cmd = "~/.config/nvim/filetype/julia/docBuild",
        })

        JuliaOpenDocs = Terminal:new({
            jobname = "Open Local Documentation",
            cmd = "browser " .. vim.fn.expand("%:p:h") .. "/docs/build/index.html & sleep 5",
        })

        JuliaOpenDocServ = Terminal:new({
            jobname = "Open Server Documentation",
            cmd = "browser http://localhost:8000 & sleep 5",
        })

        JuliaDocTests = Terminal:new({
            jobname = "Documentation Tests",
            cmd = "~/.config/nvim/filetype/julia/docTest",
        })

        JuliaTest:set_harp(1)
        JuliaREPL:set_harp(3)

    elseif vim.fn.filereadable("src/main.rs") ~= 0 then
    end
end

local projection = vim.api.nvim_create_augroup("projection", { clear = true })
vim.api.nvim_create_autocmd("VimEnter", { callback = ActivateProject, group = projection })
