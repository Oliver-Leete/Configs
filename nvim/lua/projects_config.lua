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
    templates = { "builtin", "julia", "configs" }
})

