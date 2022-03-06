local expr = mapxName.expr
local buffer = mapxName.buffer

mapxName.group(buffer, function()
    nnoremap("<localleader>i", [[<cmd>RustToggleInlayHints<cr>]], "Toggle Inlay Hints")
    nnoremap("<localleader>m", [[<cmd>RustExpandMacro<cr>]], "Expand Macro")
    nnoremap("<localleader>p", [[<cmd>RustParentModule<cr>]], "Open Parent Module")
    nnoremap("<localleader>c", [[<cmd>RustOpenCargo<cr>]], "Open Cargo")
    nnoremap("<localleader>g", [[<cmd>RustViewCrateGraph<cr>]], "View Crate Graph")

    nnoremap("<leader>mm", [[<cmd>RustRunnables<cr>]], "Run Menu")
    nnoremap("<leader>jj", [[<cmd>RustDebuggables<cr>]], "Debug Menu")
    nnoremap("<leader>ll", [[<cmd>silent !kittyOneShot maketerm "cd ']] .. vim.fn.getcwd() .. [[';cargo build --release; cargo flamegraph"<cr>]], "Make Perfdata")
end)

vim.g.projectionist_heuristics = {
    ["src/*.rs"] = {
        ["src/*.rs"] = {
            type = "source",
            alternate = "tests/{}.rs",
            related = {"benches/{}.rs", "tests/{}.rs", "docs/src/{}.md"}
        },
        ["benches/*.rs"] = {
            type = "bench",
            alternate = "src/{}.rs",
            related = {"src/{}.rs", "tests/{}.rs", "docs/src/{}.md"}
        },
        ["test/*_tests.jl"] = {
            type = "test",
            alternate = "src/{}.jl",
            related = {"benckmark/{}_benchmarks.jl", "src/{}.jl", "docs/src/{}.md"}
        },
        ["docs/src/*.md"] = {
            type = "doc",
            alternate = "src/{}.jl",
            related = {"benckmark/{}_benchmarks.jl", "test/{}_tests.jl", "src/{}.jl"}
        },
        ["README.md"] = { type = "readme" },
        ["Project.toml"] = { type = "deps" },
        ["test/runtests.jl"] = { type = "mainTest" },
        ["benchmark/benchmarks.jl"] = { type = "mainBench" },
        ["docs/src/index.md"] = { type = "mainDoc" },
    }
}
