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
        d = {
            d = {"<cmd>silent !kittydebug juliadebug<cr>", "Open Debug Terminal"},
            n = {"<cmd>silent !kittydebug juliadebug n<cr>", "Step to the Next Line"},
            N = {"<cmd>silent !kittydebug juliadebug c<cr>", "Step to the Next Breakpoint"},
            s = {"<cmd>silent !kittydebug juliadebug s<cr>", "Step In"},
            S = {"<cmd>silent !kittydebug juliadebug so<cr>", "Step Out"},
            t = {"<cmd>silent !kittydebug juliadebug bt<cr>", "Backtrace"},
            v = {"<cmd>silent !kittydebug juliadebug fr<cr>", "Variables"},
            l = {"<cmd>silent !kittydebug juliadebug st<cr>", "Status"},
            w = {"<cmd>silent !kittydebug juliadebug w<cr>", "Watchlist"},
            W = {[["<cmd>silent !kittydebug juliadebug 'w add" . input("Expression > ") . "'<cr>"]], "Add to Watchlist", expr=true},
            q = {"<cmd>silent !kittydebug juliadebug q<cr>", "Quit"},
            i = {"mzO@bp<esc>`z", "Insert Breakpoint Macro"},
            I = {"mzO@infiltrate # cond = <esc>`z", "Insert Infiltration Macro"},
            o = {"<cmd>silent !kittydebug juliadebug o<cr>", "Jump to Line in Editor"},
            ["+"] = {"<cmd>silent !kittydebug juliadebug +<cr>", "Increase Lines of Source Code"},
            ["-"] = {"<cmd>silent !kittydebug juliadebug -<cr>", "Decrease Lines of Source Code"},
            b = {[["<cmd>silent !kittydebug juliadebug 'bp add \"%:t\"\:" . line(".") . "'<cr>"]], "Set Breakpoint", expr = true},
            B = {[["<cmd>silent !kittydebug juliadebug 'bp add \"%:t\"\:" . line(".") . input("Condition > ") . "'<cr>"]], "Set Conditional Breakpoint", expr = true},
        }
    },
}, {
    buffer = 0,
})
require("which-key").register({
    ["<cr>"] = { ":<c-u>MagmaEvaluateVisual<cr>", "Evaluate Selction"},
    ["<leader>"] = {
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
