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

nnoremap("<leader>/s", "<cmd>Esource<cr>", "Source")
nnoremap("<leader>/S", "<cmd>EmainSource", "Main Source", expr)
nnoremap("<leader>/b", "<cmd>Ebench<cr>", "Benchmark")
nnoremap("<leader>/B", "<cmd>EmainBench<cr>", "Main Benchmark")
nnoremap("<leader>/t", "<cmd>Etest<cr>", "Test")
nnoremap("<leader>/T", "<cmd>EmainTest<cr>", "Main Test")
nnoremap("<leader>/p", "<cmd>Edeps<cr>", "Project Dependencies")
    nnoremap("<leader>/ns", [["<cmd>Esource " . input('File Name > ') . "<cr>"]], "Source", expr)
    nnoremap("<leader>/nb", [["<cmd>Ebench " . input('File Name > ') . "<cr>"]], "Benchmark", expr)
    nnoremap("<leader>/nB", [[<cmd>EmainBench<cr>]], "Main Benchmarks")
    nnoremap("<leader>/nt", [["<cmd>Etest " . input('File Name > ') . "<cr>"]], "Test", expr)
    nnoremap("<leader>/nT", [[<cmd>EmainTest<cr>]], "Main Tests")
end)

vim.g.projectionist_heuristics = {
    ["src/*.rs"] = {
        ["src/*.rs"] = {
            type = "source",
            alternate = "tests/{}.rs",
            related = {"benches/{}.rs", "tests/{}.rs"}
        },
        ["benches/*.rs"] = {
            type = "bench",
            alternate = "src/{}.rs",
            related = {"src/{}.rs", "tests/{}.rs"}
        },
        ["tests/*.rs"] = {
            type = "test",
            alternate = "src/{}.rs",
            related = {"src/{}.rs", "benches/{}.rs"}
        },
        ["README.md"] = { type = "readme" },
        ["Cargo.toml"] = { type = "deps" },
        ["src/main.rs"] = { type = "mainSource" },
        ["tests/main.rs"] = { type = "mainTest" },
        ["benches/main.rs"] = { type = "mainBench" },
    }
}
