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
            "toggleterm.attach_toggleterm",
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
        }

    },
    templates = { "builtin", "julia", "configs" }
})

overseer.register_template({
    name = "View LSP Logs",
    builder = function()
        return {
            name = "View LSP Logs",
            cmd = "tail --follow --retry ~/.local/state/nvim/lsp.log | less -S",
        }
    end,
    priority = 6000,
    params = {},
})
overseer.register_template({
    name = "View Neovim Logs",
    builder = function()
        return {
            name = "View Neovim Logs",
            cmd = "tail --follow --retry ~/.local/state/nvim/log | less -S",
        }
    end,
    priority = 6000,
    params = {},
})
overseer.register_template({
    generator = function()
        local logHandler = io.popen(
            [[fd -e log]]
        )
        local ret = {}
        if logHandler then
            local logs = logHandler:read("*a")
            logHandler:close()
            for log in logs:gmatch("([^\r\n]+)") do
                table.insert(
                    ret,
                    {
                        name = "Show " .. log,
                        builder = function()
                            return {
                                name = "Show " .. log,
                                cmd = "tail --follow --retry " .. log,
                            }
                        end,
                        priority = 1000,
                        params = {},
                    }
                )
            end
        end
        return ret
    end
})
