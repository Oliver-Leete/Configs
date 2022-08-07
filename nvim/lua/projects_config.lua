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
        [[rg --no-filename --no-heading --no-line-number -e "^\s*@testcase\s*\"(.*)\"\s*begin.*\$" -r "\$1"]]
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
                    JuliaTest:send_open([[PackageTests.runtests("]] .. name .. [[",spin=false)]], true, 1)
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
            id = 7,
        })

        JuliaREPL = Terminal:new({
            jobname = "Julia REPL",
            cmd = "julia",
            id = 8,
        })

        JuliaLiveDocs = Terminal:new({
            jobname = "Julia Doc Server",
            cmd = [[julia --project=docs -ie 'using ]] ..
                vim.g.project .. [[, LiveServer; servedocs(launch_browser=true)']],
            runnable = { source = "Julia", name = "Julia Doc Server", func = function() JuliaLiveDocs:set_toggle(4) end },
            id = 9,
        })

        JuliaPrecompile = Terminal:new({
            jobname = "Package Precompile",
            cmd = "~/.config/nvim/filetype/julia/precompile",
            id = 10,
        })

        JuliaBuildDocs = Terminal:new({
            jobname = "Build Documentation",
            cmd = "~/.config/nvim/filetype/julia/docBuild",
            id = 11,
        })

        JuliaOpenDocs = Terminal:new({
            jobname = "Open Local Documentation",
            cmd = "browser " .. vim.fn.expand("%:p:h") .. "/docs/build/index.html & sleep 5",
            id = 12,
        })

        JuliaOpenDocServ = Terminal:new({
            jobname = "Open Server Documentation",
            cmd = "browser http://localhost:8000 & sleep 5",
            id = 13,
        })

        JuliaDocTests = Terminal:new({
            jobname = "Documentation Tests",
            cmd = "~/.config/nvim/filetype/julia/docTest",
            id = 14,
        })

        JuliaTest:set_harp(1)
        JuliaREPL:set_harp(3)

    elseif vim.fn.filereadable("src/main.rs") ~= 0 then
    end
end

local projection = vim.api.nvim_create_augroup("projection", { clear = true })
vim.api.nvim_create_autocmd("VimEnter", { callback = ActivateProject, group = projection })

require("neotest").setup({
    consumers = {
        overseer = require("neotest.consumers.overseer"),
    },
    overseer = {
        enabled = true,
        force_default = true,
    },
    adapters = {
        require("neotest-rust"),
        require("neotest-python"),
        require("neotest-julia-retest"),
        require("neotest-julia-benchmarktools")
    },
    floating = {
        border = "rounded",
        max_height = 0.9,
        max_width = 0.9,
        options = {},
    },
    mappings = {
        expand = { "<CR>", "<2-LeftMouse>" },
        expand_all = "e",
        output = "p",
        short = "P",
        attach = "a",
        jumpto = "i",
        stop = "u",
        run = "r",
        mark = "m",
        run_marked = "R",
        clear_marked = "M",
        target = "t",
        clear_target = "T",
    },

})

local overseer = require("overseer")
overseer.setup({
    form = { win_opts = { winblend = 0, }, },
    task_editor = { win_opts = { winblend = 0, }, },
    task_win = { win_opts = { winblend = 0, }, },
    confirm = { win_opts = { winblend = 0, }, },
    bindings = {
        ["?"] = "ShowHelp",
        ["<CR>"] = "RunAction",
        ["<C-e>"] = "Edit",
        ["o"] = "Open",
        ["<C-v>"] = "OpenVsplit",
        ["<C-f>"] = "OpenFloat",
        ["p"] = "TogglePreview",
        ["<C-l>"] = "IncreaseDetail",
        ["<C-h>"] = "DecreaseDetail",
        ["L"] = "IncreaseAllDetail",
        ["H"] = "DecreaseAllDetail",
        ["["] = "DecreaseWidth",
        ["]"] = "IncreaseWidth",
        ["{"] = "PrevTask",
        ["}"] = "NextTask",
    },
    component_aliases = {
        default_neotest = {
            "on_output_summarize",
            "on_exit_set_status",
            "on_complete_notify",
            "on_complete_dispose",
        },
    },
})

overseer.register_template({
    name = "Reload XMonad",
    builder = function()
        return {
            name = "Reload XMonad",
            cwd = "/home/oleete/.config/xmonad",
            cmd = "/home/oleete/.config/bin/xmonadRebuild",
        }
    end,
    priority = 1000,
    desc = "Recompile and reload XMonad",
    tags = { overseer.TAG.BUILD },
    params = {},
    condition = {
        dir = "/home/oleete/.config",
    },
})

overseer.register_template({
    name = "Reload Kitty",
    builder = function()
        return {
            name = "Reload Kitty",
            cmd = "pkill -10 kitty",
        }
    end,
    priority = 1000,
    desc = "Refresh the config of the current session",
    params = {},
    condition = {
        dir = "/home/oleete/.config",
    },
})

overseer.register_template({
    name = "Source File",
    builder = function()
        return {
            name = "Source File",
            cmd = "nvrStart +'source %'",
        }
    end,
    priority = 1000,
    desc = "Source the current lua file",
    params = {},
    condition = {
        dir = "/home/oleete/.config",
        filetype = "lua",
    },
})

overseer.register_template({
    name = "Reload Neovim",
    builder = function()
        return {
            name = "Reload Neovim",
            cmd = "nvrStart +'source! /home/oleete/.config/nvim/init.lua'",
        }
    end,
    priority = 1000,
    desc = "Source init.lua",
    params = {},
    condition = {
        dir = "/home/oleete/.config",
        filetype = "lua",
    },
})

local function is_julia_project()
    if vim.fn.filereadable("src/" .. vim.g.project .. ".jl") ~= 0 then
        return true
    end
end

overseer.register_template({
    name = "Julia Doc Server",
    builder = function()
        return {
            name = "Julia Doc Server",
            cmd = [[julia --project=docs -ie 'using ]] ..
                vim.g.project .. [[, LiveServer; servedocs(launch_browser=true)']]
        }
    end,
    priority = 100,
    desc = "Start up the documentation server",
    params = {},
    condition = {
        callback = is_julia_project
    },
})

overseer.register_template({
    name = "Package Precompile",
    builder = function()
        return {
            name = "Package Precompile",
            cmd = "~/.config/nvim/filetype/julia/precompile",
        }
    end,
    priority = 100,
    params = {},
    condition = {
        callback = is_julia_project
    },
})

overseer.register_template({
    name = "Build Documentation",
    builder = function()
        return {
            name = "Build Documentation",
            cmd = "~/.config/nvim/filetype/julia/docBuild",
        }
    end,
    priority = 100,
    desc = "Do a single shot compilation of the documentation",
    params = {},
    condition = {
        callback = is_julia_project
    },
})

overseer.register_template({
    name = "Open Local Documentation",
    builder = function()
        return {
            name = "Open Local Documentation",
            cmd = "browser " .. vim.fn.expand("%:p:h") .. "/docs/build/index.html & sleep 5",
        }
    end,
    priority = 100,
    desc = "Open the local documentation",
    params = {},
    condition = {
        callback = is_julia_project
    },
})

overseer.register_template({
    name = "Open Server Documentation",
    builder = function()
        return {
            name = "Open Server Documentation",
            cmd = "browser http://localhost:8000 & sleep 5",
        }
    end,
    priority = 100,
    params = {},
    condition = {
        callback = is_julia_project
    },
})

overseer.register_template({
    name = "Documentation Tests",
    builder = function()
        return {
            name = "Documentation Tests",
            cmd = "~/.config/nvim/filetype/julia/docTest",
        }
    end,
    priority = 100,
    desc = "Run the julia documentation test",
    params = {},
    condition = {
        callback = is_julia_project
    },
})

