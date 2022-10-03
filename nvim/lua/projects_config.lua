local handle = io.popen([[echo "$(basename "$PWD")"]])
local project
if handle then
    project = handle:read("*a")
    handle:close()
    vim.g.project = string.gsub(project, "\n", "")
else
    vim.notify("Couldn't find project name", "Warn", { title = "Projects" })
end

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
            related = { "test/{}_tests.jl", "benchmark/{}_benchmarks.jl", "docs/src/{}.md" },
        },
        ["test/*_tests.jl"] = {
            type = "test",
            alternate = "src/{}.jl",
            related = { "src/{}.jl", "benchmark/{}_benchmarks.jl", "docs/src/{}.md" },
        },
        ["benches/*_benchmarks.jl"] = {
            type = "bench",
            alternate = "src/{}.jl",
            related = { "src/{}.jl", "tests/{}_tests.jl", "docs/src/{}.md" },
        },
        ["docs/src/*.md"] = {
            type = "doc",
            alternate = "src/{}.jl",
            related = { "test/{}_tests.jl", "benchmark/{}_benchmarks.jl", "src/{}.jl" },
        },

        ["src/" .. vim.g.project .. ".jl"] = {
            type = "mainSource",
            alternate = "test/PackageTests.jl",
            related = { "test/PackageTests.jl", "benchmark/PackageBenchmarks.jl", "docs/make.jl" },
        },
        ["test/PackageTests.jl"] = {
            type = "mainTest",
            alternate = "src/" .. vim.g.project .. ".jl",
            related = { "src/" .. vim.g.project .. ".jl", "benchmark/PackageBenchmarks.jl", "docs/make.jl" },
        },
        ["benchmark/PackageBenchmarks.jl"] = {
            type = "mainBench",
            alternate = "src/" .. vim.g.project .. ".jl",
            related = { "src/" .. vim.g.project .. ".jl", "test/PackageTests.jl", "docs/make.jl" },
        },
        ["docs/make.jl"] = {
            type = "mainDoc",
            alternate = "src/" .. vim.g.project .. ".jl",
            related = { "src/" .. vim.g.project .. ".jl", "test/PackageTests.jl", "benchmark/PackageBenchmarks.jl" },
        },

        ["README.md"] = { type = "readme" },
        ["Project.toml"] = { type = "deps" },
        type = "julia",
    },
}


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
        require("neotest.adapters.neotest-julia-retest"),
        require("neotest.adapters.neotest-julia-benchmarktools")
    },
    floating = {
        border = Border,
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
local STATUS = require("overseer.constants").STATUS
overseer.setup({
    form = { border = Border, win_opts = { winblend = 0, }, },
    task_editor = { border = Border, win_opts = { winblend = 0, }, },
    task_win = { border = Border, win_opts = { winblend = 0, }, },
    confirm = { border = Border, win_opts = { winblend = 0, }, },
    task_list = {
        bindings = {
            ["?"] = "ShowHelp",
            ["<CR>"] = "RunAction",
            ["<C-e>"] = "Edit",
            ["o"] = "<cmd>OverseerQuickAction open in toggleterm<cr>",
            ["p"] = "TogglePreview",
            ["<C-l>"] = "IncreaseDetail",
            ["<C-h>"] = "DecreaseDetail",
            ["L"] = "IncreaseAllDetail",
            ["H"] = "DecreaseAllDetail",
            ["["] = "PrevTask",
            ["]"] = "NextTask",
            ["{"] = nil,
            ["}"] = nil,
            ["<C-v>"] = nil,
            ["<C-f>"] = nil,
        },
    },
    component_aliases = {
        default_neotest = {
            "on_output_summarize",
            "on_exit_set_status",
            "on_complete_notify",
            "on_complete_dispose",
            { "toggleterm.attach_toggleterm", goto_prev = true },
            "unique",
        },
        default = {
            "on_output_summarize",
            "on_exit_set_status",
            "on_complete_notify",
            "on_complete_dispose",
            "toggleterm.attach_toggleterm",
        },
    },
    actions = {
        ["open"] = false,
        ["open vsplit"] = false,
        ["open hsplit"] = false,
        ["set loclist diagnostics"] = false,
        ["open in toggleterm"] = {
            desc = "Attach this task to a toggleterm terminal",
            run = function(task)
                if task.toggleterm then
                    if task.toggleterm:is_open() then
                        task.toggleterm:close()
                        task.toggleterm:open()
                    else
                        task.toggleterm:open()
                    end
                else
                    local bufnr = task.strategy.bufnr
                    task.toggleterm = Terminal:new({ bufnr = bufnr, jobname = task.name })
                    task:add_components({ "toggleterm.on_dispose_clean_toggleterm" })
                    task.toggleterm:toggle()
                    task.toggleterm:__resurrect()
                end
                task.toggleterm:set_harp(2)
            end,
        },
        ["keep runnning"] = {
            desc = "restart the task even if it succeeds",
            run = function(task)
                task:add_components({ { "on_complete_restart", statuses = { STATUS.FAILURE, STATUS.SUCCESS } } })
                if task.status == STATUS.FAILURE or task.status == STATUS.SUCCESS then
                    task:restart()
                end
            end
        },
        ["don't dispose"] = {
            desc = "keep the task until manually disposed",
            run = function(task)
                task:remove_components({ "on_complete_dispose" })
            end
        },
        ["hard restart"] = {
            desc = "restart the server with the task",
            run = function(task)
                local task_list = require("overseer.task_list").list_tasks()
                for _, ntask in pairs(task_list) do
                    if ntask.metadata.is_test_server then
                        ntask:restart(true)
                    end
                end
                task:restart(true)
            end,
            condition = function(task)
                local not_pend = task.status ~= STATUS.PENDING
                return task.metadata.uses_server and not_pend
            end,
        },
        ["dump task"] = {
            desc = "save task table to DumpTask (for debugging)",
            run = function(task)
                DumpTask = task
            end,
        }
    },
    templates = { "builtin", "julia", "configs", "runners", "run_bins", "logs" }
})

overseer.register_template({
    name = "View LSP Logs",
    builder = function()
        return {
            name = "View LSP Logs",
            cmd = "tail --follow --retry ~/.local/state/nvim/lsp.log | less -S",
            components = { "default", "unique" },
        }
    end,
    priority = 150,
    params = {},
})
overseer.register_template({
    name = "View Neovim Logs",
    builder = function()
        return {
            name = "View Neovim Logs",
            cmd = "tail --follow --retry ~/.local/state/nvim/log | less -S",
            components = { "default", "unique" }
        }
    end,
    priority = 150,
    params = {},
})
overseer.register_template({
    name = "Plot from logfile",
    params = {
        key = {
            type = "string",
            name = "Key",
            desc = "A search term to find the desired parameter to plot",
            optional = false,
        }
    },
    builder = function(params)
        return {
            name = "Plot " .. params.key,
            cmd = [[
                echo temp > /tmp/T.csv; rg ']] ..
                params.key ..
                [[' /home/oleete/Projects/PowderModel/test/test_outputs/full_out.log | rg -o '[0-9.]*$' >> /tmp/T.csv;
                julia -e '
                    using Plots, CSV;
                    ENV["GKSwstype"]="nul"
                    gr()
                    a = CSV.File("/tmp/T.csv")
                    savefig(plot([a[i][1] for i in 1:length(a)]), "/tmp/T.png")
                '
                feh /tmp/T.png
            ]],
            components = { "default", "unique" }
        }
    end,
    priority = 150,
    condition = {
        dir = "/home/oleete/Projects/PowderModel"
    }
})
overseer.register_template({
    name = "View Animation",
    builder = function()
        return {
            name = "Animation",
            cmd = "mpv --loop-file=inf /tmp/fig.gif",
            components = { "default", "unique" }
        }
    end,
    priority = 151,
    condition = {
        dir = "/home/oleete/Projects/PowderModel"
    }
})

overseer.register_template({
    name = "System Info (btop)",
    builder = function()
        return {
            name = "btop",
            cmd = "btop",
            components = { "default", "unique" }
        }
    end,
    priority = 155,
    params = {},
})
overseer.register_template({
    name = "Lazygit",
    builder = function()
        return {
            name = "lazygit",
            cmd = "lazygit",
            components = { "default", "unique" }
        }
    end,
    priority = 2,
    params = {},
})

overseer.register_template({
    name = "Build Document",
    builder = function()
        return {
            name = "Build Document",
            cmd = "latexmk -pdf -file-line-error -synctex=1 OML-Thesis.tex",
            components = {
                "on_output_summarize",
                "on_exit_set_status",
                "on_complete_notify",
                "on_complete_dispose",
                "unique",
                { "toggleterm.attach_toggleterm", hide = true },
            }
        }
    end,
    priority = 5,
    condition = {
        dir = "/home/oleete/UniversityDrive/Thesis/thesis"
    }
})

OvTermNum = 0
overseer.register_template({
    name = "Fish",
    builder = function()
        OvTermNum = OvTermNum + 1
        return {
            name = "Fish " .. OvTermNum,
            cmd = "fish",
            components = {
                "on_output_summarize",
                "on_exit_set_status",
                "on_complete_notify",
                "on_complete_dispose",
                { "toggleterm.attach_toggleterm", num = 1 },
            }
        }
    end,
    priority = 1,
    params = {},
})
