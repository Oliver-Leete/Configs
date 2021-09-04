vim.api.nvim_set_option("errorformat", [[%tRROR:\ %m\ at\ %f:%l,%-G%.%#]])
vim.api.nvim_set_option("makeprg", [[julia\ -e\ \'using\ Pkg;\ Pkg.precompile()\']])
vim.api.nvim_buf_set_option(0, "commentstring", [[#%s]])

vim.api.nvim_buf_set_var(0, "replcommand", "julia")

require("which-key").register({
    -- ["<cr>"] = { "<cmd>MagmaEvaluateOperator<cr>", "Evaluate Line"},
    ["<cr>"] = { "<cmd>SlimeSendCurrentLine<cr>", "Evaluate Line"},
    ["<leader>"] = {
        ["/"] = {
            S = { [["<cmd>Esource " . split(getcwd(), '/')[-1] . "<cr>"]], "Main Source", expr = true },
            v = {
                S = { [["<cmd>Esource " . split(getcwd(), '/')[-1] . "<cr>"]], "Main Source", expr = true },
            },
            x = {
                S = { [["<cmd>Ssource " . split(getcwd(), '/')[-1] . "<cr>"]], "Main Source", expr = true },
            },
            T = {
                S = { [["<cmd>Ssource " . split(getcwd(), '/')[-1] . "<cr>"]], "Main Source", expr = true },
            },
            n = {
                S = { [["<cmd>Esource " . split(getcwd(), '/')[-1] . "<cr>"]], "Main Source", expr = true },
            },
        },
        t = {
            i = { "<cmd>silent !echo using Revise | kittyrepl julia<cr>", "REPL Terminal" },
            d = { "<cmd>silent !echo using Revise, Debugger | kittyrepl julia<cr>", "REPL Terminal" },
        },
        I = { "<cmd>MagmaInit<cr>", "Start Jupyter"},
        i = {
            i = { "<cmd>MagmaEvaluateLine<cr>", "Evaluate Line"},
            r = { "<cmd>MagmaReevaluateCell<cr>", "Re-Evaluate Cell"},
            d = { "<cmd>MagmaDelete<cr>", "Delete Cell"},
        },
        v = {
            i = { "<cmd>MagmaShowOutput<cr>", "Evaluate Line"},
        },
        r = {
            d = {
                [[[fyyO<esc><cmd>.!cat ~/.config/nvim/filetype/julia/func_docstring.txt<cr>pdw>>/TODO:<cr>]],
                "Make Docstring",
                noremap = false,
            },
        },
        m = {
            m = { [[<cmd>silent !kittymake "~/.config/nvim/filetype/julia/precompile"<cr>]], "Precompile" },
            t = { [[<cmd>silent !kittymake "~/.config/nvim/filetype/julia/test"<cr>]], "Test Package" },
            c = { [[<cmd>silent !kittymake "~/.config/nvim/filetype/julia/testCov"<cr>]], "Coverage Check Package" },
            b = { [[<cmd>silent !kittymake "~/.config/nvim/filetype/julia/benchmark"<cr>]], "Benckmark Package" },
            d = { [[<cmd>silent !kittymake "~/.config/nvim/filetype/julia/docBuild"<cr>]], "Build Package Documentation" },
        },
    },
}, {
    buffer = 0,
})
require("which-key").register({
    ["<cr>"] = { ":<c-u>MagmaEvaluateVisual<cr>", "Evaluate Selction"},
    ["<leader>"] = {
        i = {
            i = { ":<c-u>MagmaEvaluateVisual<cr>", "Evaluate Selction"},
        }
    },
}, {
    buffer = 0,
    mode = "x",
})

vim.g.projectionist_heuristics = {
    ["src/*.jl"] = {
        ["src/*.jl"] = {
            type = "source",
            alternate = "test/{}_tests.jl",
            related = {"benckmark/{}_benchmarks.jl", "test/{}_tests.jl", "docs/src/{}.md"}
        },
        ["benchmark/*_benchmarks.jl"] = {
            type = "bench",
            alternate = "src/{}.jl",
            related = {"src/{}.jl", "test/{}_tests.jl", "docs/src/{}.md"}
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

vim.fn["textobj#sentence#init"]()
