vim.api.nvim_set_option("errorformat", [[%tRROR:\ %m\ at\ %f:%l,%-G%.%#]])
vim.api.nvim_set_option("makeprg", [[julia\ -e\ \'using\ Pkg;\ Pkg.precompile()\']])
vim.api.nvim_buf_set_option(0, "commentstring", [[#%s]])

vim.api.nvim_buf_set_var(0, "replCommand", "juliaREPL")
vim.api.nvim_buf_set_var(0, "debugCommand", "juliadebug")

require("which-key").register({
    -- ["<cr>"] = { "<cmd>MagmaEvaluateOperator<cr>", "Evaluate Line"},
    ["<cr>"] = { "<cmd>SlimeSendCurrentLine<cr>", "Send Line to Repl"},
    ["<localleader>"] = {
        ["<cr>"] = { "<cmd>MagmaEvaluateOperator<cr>", "Evaluate Line"},
        i = {
            i = {"<cmd>silent !kittycommand infilterm juliainfil<cr>", "Open Debug Terminal"},
            n = {"<cmd>silent !kittycommand infilterm juliainfil @continue<cr>", "Continue to Next Breakpoint"},
            b = {"mzO@infiltrate<esc>`z", "Insert Breakpoint"},
            B = {"mzO@infiltrate # cond = ", "Insert Conditional Breakpoint"},
            I = {"<cmd>silent !kittycommand infilterm juliainfil @toggle<cr>", "Toggle Breakpoint"},
            t = {"<cmd>silent !kittycommand infilterm juliainfil @trace<cr>", "Backtrace"},
            v = {"<cmd>silent !kittycommand infilterm juliainfil @locals<cr>", "Variables"},
            V = {"<cmd>silent !kittycommand infilterm juliainfil @exfiltrate<cr>", "Save Variables"},
            d = {[["<cmd>silent !kittycommand infilterm juliainfil '@descend " . input("Descend Into? > ") . "'<cr>"]], "Cthulu's Madness"},
            D = {[["<cmd>silent !kittycommand infilterm juliainfil '@descend " . getline(".") . "'<cr>"]], "Cthulu's Madness (Line)"},
            w = {[["<cmd>silent !kittycommand infilterm juliainfil '@descend_code_warntype " . input("Descend Into ? ") . "'<cr>"]], "Cthulu's Warning"},
            W = {[["<cmd>silent !kittycommand infilterm juliainfil '@descend_code_warntype " . getline(".") . "'<cr>"]], "Cthulu's Warning (Line)"},
            q = {"<cmd>silent !kittycommand infilterm juliainfil @exit<cr>", "Quit"},
        },
    },
    ["<leader>"] = {
        ["/"] = {
            d = { "<cmd>Edoc<cr>", "Documentation" },
            D = { "<cmd>EmainDoc<cr>", "Main Documentation" },
            s = { "<cmd>Esource<cr>", "Source" },
            b = { "<cmd>Ebench<cr>", "Benchmark" },
            B = { "<cmd>EmainBench<cr>", "Main Benchmark" },
            t = { "<cmd>Etest<cr>", "Test" },
            T = { "<cmd>EmainTest<cr>", "Main Test" },
            p = { "<cmd>Edeps<cr>", "Project Dependencies" },
            S = { [["<cmd>Esource " . split(getcwd(), '/')[-1] . "<cr>"]], "Main Source", expr = true },
            v = {
                S = { [["<cmd>Esource " . split(getcwd(), '/')[-1] . "<cr>"]], "Main Source", expr = true },
                d = { "<cmd>Vdoc<cr>", "Documentation" },
                D = { "<cmd>VmainDoc<cr>", "Main Documentation" },
                s = { "<cmd>Vsource<cr>", "Source" },
                b = { "<cmd>Vbench<cr>", "Benchmark" },
                B = { "<cmd>VmainBench<cr>", "Main Benchmark" },
                t = { "<cmd>Vtest<cr>", "Test" },
                T = { "<cmd>VmainTest<cr>", "Main Test" },
                p = { "<cmd>Vdeps<cr>", "Project Dependencies" },
            },
            x = {
                S = { [["<cmd>Ssource " . split(getcwd(), '/')[-1] . "<cr>"]], "Main Source", expr = true },
                d = { "<cmd>Sdoc<cr>", "Documentation" },
                D = { "<cmd>SmainDoc<cr>", "Main Documentation" },
                s = { "<cmd>Ssource<cr>", "Source" },
                b = { "<cmd>Sbench<cr>", "Benchmark" },
                B = { "<cmd>SmainBench<cr>", "Main Benchmark" },
                t = { "<cmd>Stest<cr>", "Test" },
                T = { "<cmd>SmainTest<cr>", "Main Test" },
                p = { "<cmd>Sdeps<cr>", "Project Dependencies" },
            },
            O = {
                S = { [["<cmd>Ssource " . split(getcwd(), '/')[-1] . "<cr>"]], "Main Source", expr = true },
                d = { "<cmd>Tdoc<cr>", "Documentation" },
                D = { "<cmd>TmainDoc<cr>", "Main Documentation" },
                s = { "<cmd>Tsource<cr>", "Source" },
                b = { "<cmd>Tbench<cr>", "Benchmark" },
                B = { "<cmd>TmainBench<cr>", "Main Benchmark" },
                t = { "<cmd>Ttest<cr>", "Test" },
                T = { "<cmd>TmainTest<cr>", "Main Test" },
                p = { "<cmd>Tdeps<cr>", "Project Dependencies" },
            },
            n = {
                S = { [["<cmd>Esource " . split(getcwd(), '/')[-1] . "<cr>"]], "Main Source", expr = true },
                d = { [["<cmd>Edoc " . input('File Name > ') . "<cr>"]], "Documentation", expr = true },
                D = { [[<cmd>EmainDoc<cr>]], "Main Documentation" },
                s = { [["<cmd>Esource " . input('File Name > ') . "<cr>"]], "Source", expr = true },
                b = { [["<cmd>Ebench " . input('File Name > ') . "<cr>"]], "Benchmark", expr = true },
                B = { [[<cmd>EmainBench<cr>]], "Main Benchmarks" },
                t = { [["<cmd>Etest " . input('File Name > ') . "<cr>"]], "Test", expr = true },
                T = { [[<cmd>EmainTest<cr>]], "Main Tests" },
                p = { [[<cmd>Edeps<cr>]], "Project Dependencies" },
            },
        },
        r = {
            d = {
                [[[fyyO<esc><cmd>.!cat ~/.config/nvim/filetype/julia/func_docstring.txt<cr>pdw>>/TODO:<cr>]],
                "Make Docstring",
                noremap = false,
            },
        },
        m = {
            m = { [[<cmd>silent !kittyterm maketerm "~/.config/nvim/filetype/julia/precompile"<cr>]], "Precompile" },
            t = { [[<cmd>silent !kittyterm maketerm "~/.config/nvim/filetype/julia/test"<cr>]], "Test Package" },
            c = { [[<cmd>silent !kittyterm maketerm "~/.config/nvim/filetype/julia/testCov"<cr>]], "Coverage Check Package" },
            b = { [[<cmd>silent !kittyterm maketerm "~/.config/nvim/filetype/julia/benchmark"<cr>]], "Benckmark Package" },
            d = { [[<cmd>silent !kittyterm maketerm "~/.config/nvim/filetype/julia/docBuild"<cr>]], "Build Package Documentation" },
        },
        d = {
            d = {"<cmd>silent !kittycommand debugterm juliadebug<cr>", "Open Debug Terminal"},
            e = {[["<cmd>silent !kittycommand debugterm juliadebug '@enter . input("Debug? > ") . <cr>"]], "Enter Function"},
            E = {[["<cmd>silent !kittycommand debugterm juliadebug '@enter . getline(".") . <cr>"]], "Enter Function (Line)"},
            n = {"<cmd>silent !kittycommand debugterm juliadebug n<cr>", "Step to the Next Line"},
            N = {"<cmd>silent !kittycommand debugterm juliadebug c<cr>", "Step to the Next Breakpoint"},
            s = {"<cmd>silent !kittycommand debugterm juliadebug s<cr>", "Step In"},
            S = {"<cmd>silent !kittycommand debugterm juliadebug so<cr>", "Step Out"},
            t = {"<cmd>silent !kittycommand debugterm juliadebug bt<cr>", "Backtrace"},
            v = {"<cmd>silent !kittycommand debugterm juliadebug fr<cr>", "Variables"},
            l = {"<cmd>silent !kittycommand debugterm juliadebug st<cr>", "Status"},
            w = {"<cmd>silent !kittycommand debugterm juliadebug w<cr>", "Watchlist"},
            c = {"<cmd>silent !kittycommand debugterm juliadebug C<cr>", "Switch to Compiled Mode"},
            W = {[["<cmd>silent !kittycommand debugterm juliadebug 'w add " . input("Expression > ") . "'<cr>"]], "Add to Watchlist", expr=true},
            q = {"<cmd>silent !kittycommand debugterm juliadebug q<cr>", "Quit"},
            i = {"mzO@bp<esc>`z", "Insert Breakpoint Macro"},
            o = {"<cmd>silent !kittycommand debugterm juliadebug o<cr>", "Jump to Line in Editor"},
            ["+"] = {"<cmd>silent !kittycommand debugterm juliadebug +<cr>", "Increase Lines of Source Code"},
            ["-"] = {"<cmd>silent !kittycommand debugterm juliadebug -<cr>", "Decrease Lines of Source Code"},
            b = {[["<cmd>silent !kittycommand debugterm juliadebug 'bp add \"%:t\"\:" . line(".") . "'<cr>"]], "Set Breakpoint", expr = true},
            B = {[["<cmd>silent !kittycommand debugterm juliadebug 'bp add \"%:t\"\:" . line(".") . " " . input("Condition > ") . "'<cr>"]], "Set Conditional Breakpoint", expr = true},
            r = {
                r = {"<cmd>silent !kittycommand debugterm juliadebug bp<cr>", "List Breakpoints"},
                b = {[["<cmd>silent !kittycommand debugterm juliadebug bp rm " . input("Point to Remove > ") . "<cr>"]], "Remove Breapoint"},
                w = {[["<cmd>silent !kittycommand debugterm juliadebug w rm " . input("Item to Remove > ") . "<cr>"]], "Remove Watchlist"},
            },
        }
    },
}, {
    buffer = 0,
})
require("which-key").register({
    ["<cr>"] = { "<Plug>SlimeRegionSend", "Send to Repl"},
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
