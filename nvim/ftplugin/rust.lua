Map("n", "<localleader>i", [[<cmd>RustToggleInlayHints<cr>]], { buffer = 0 })
Map("n", "<localleader>m", [[<cmd>RustExpandMacro<cr>]], { buffer = 0 })
Map("n", "<localleader>p", [[<cmd>RustParentModule<cr>]], { buffer = 0 })
Map("n", "<localleader>c", [[<cmd>RustOpenCargo<cr>]], { buffer = 0 })
Map("n", "<localleader>g", [[<cmd>RustViewCrateGraph<cr>]], { buffer = 0 })

Map("n", "<leader>mm", [[<cmd>RustRunnables<cr>]], { buffer = 0 })
Map("n", "<leader>jj", [[<cmd>RustDebuggables<cr>]], { buffer = 0 })
Map(
    "n",
    "<leader>ll",
    [[<cmd>silent !kittyOneShot maketerm "cd ']]
    .. vim.fn.getcwd()
    .. [[';cargo build --release; cargo flamegraph"<cr>]],
    { buffer = 0 }
)

Map("n", "<leader>/s", "<cmd>silent Esource<cr>", { buffer = 0 })
Map("n", "<leader>/S", "<cmd>silent EmainSource", { buffer = 0, expr = 0 })
Map("n", "<leader>/b", "<cmd>silent Ebench<cr>", { buffer = 0 })
Map("n", "<leader>/B", "<cmd>silent EmainBench<cr>", { buffer = 0 })
Map("n", "<leader>/t", "<cmd>silent Etest<cr>", { buffer = 0 })
Map("n", "<leader>/T", "<cmd>silent EmainTest<cr>", { buffer = 0 })
Map("n", "<leader>/p", "<cmd>silent Edeps<cr>", { buffer = 0 })

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
