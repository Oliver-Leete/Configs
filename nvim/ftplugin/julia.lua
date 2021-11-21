vim.cmd([[set errorformat=%E%tRROR:%m]])
-- vim.cmd([[set errorformat+=%Zin\ expression\ starting\ at\ %f:%l]])
vim.cmd([[set errorformat+=%-G%.%#@\ /.%#]])
vim.cmd([[set errorformat+=%Z%.%#@\ %f:%l]])
vim.cmd([[set errorformat+=%C%.%#]])
vim.cmd([[set errorformat+=%-G%.%#]])
vim.api.nvim_set_option("makeprg", [[julia\ -e\ \'using\ Pkg;\ Pkg.precompile()\']])
vim.api.nvim_buf_set_option(0, "commentstring", [[#%s]])

vim.api.nvim_buf_set_var(0, "replCommand", "juliaREPL")
vim.api.nvim_buf_set_var(0, "debugCommand", "juliadebug")

local expr = mapxName.expr
local buffer = mapxName.buffer

mapxName.group(buffer, function()
mapxName.name("<localleader>i", "Infil Debugging")
nnoremap("<localleader>ii", "<cmd>silent !kittyPersistent infilterm juliainfil<cr>", "Open Debug Terminal")
nnoremap("<localleader>in", "<cmd>silent !kittyPersistent infilterm juliainfil @continue<cr>", "Continue to Next Breakpoint")
nnoremap("<localleader>ib", "m1O@infiltrate<esc>`1", "Insert Breakpoint")
nnoremap("<localleader>iB", "O@infiltrate # cond = ", "Insert Conditional Breakpoint")
nnoremap("<localleader>iI", "<cmd>silent !kittyPersistent infilterm juliainfil @toggle<cr>", "Toggle Breakpoint")
nnoremap("<localleader>it", "<cmd>silent !kittyPersistent infilterm juliainfil @trace<cr>", "Backtrace")
nnoremap("<localleader>iv", "<cmd>silent !kittyPersistent infilterm juliainfil @locals<cr>", "Variables")
nnoremap("<localleader>iV", "<cmd>silent !kittyPersistent infilterm juliainfil @exfiltrate<cr>", "Save Variables")
nnoremap("<localleader>id", [["<cmd>silent !kittyPersistent infilterm juliainfil '@descend " . input("Descend Into? > ") . "'<cr>"]], "Cthulu's Madness")
nnoremap("<localleader>iD", [["<cmd>silent !kittyPersistent infilterm juliainfil '@descend " . getline(".") . "'<cr>"]], "Cthulu's Madness (Line)")
nnoremap("<localleader>iw", [["<cmd>silent !kittyPersistent infilterm juliainfil '@descend_code_warntype " . input("Descend Into ? ") . "'<cr>"]], "Cthulu's Warning")
nnoremap("<localleader>iW", [["<cmd>silent !kittyPersistent infilterm juliainfil '@descend_code_warntype " . getline(".") . "'<cr>"]], "Cthulu's Warning (Line)")
nnoremap("<localleader>iq", "<cmd>silent !kittyPersistent infilterm juliainfil @exit<cr>", "Quit")

nnoremap("<leader>mm", [[<cmd>silent !kittyOneShot maketerm "~/.config/nvim/filetype/julia/precompile"<cr>]], "Precompile")
nnoremap("<leader>mt", [[<cmd>silent !kittyOneShot maketerm "~/.config/nvim/filetype/julia/test"<cr>]], "Test Package")
nnoremap("<leader>mc", [[<cmd>silent !kittyOneShot maketerm "~/.config/nvim/filetype/julia/testCov"<cr>]], "Coverage Check Package")
nnoremap("<leader>mb", [[<cmd>silent !kittyOneShot maketerm "~/.config/nvim/filetype/julia/benchmark"<cr>]], "Benckmark Package")
nnoremap("<leader>md", [[<cmd>silent !kittyOneShot maketerm "~/.config/nvim/filetype/julia/docBuild"<cr>]], "Build Package Documentation")
nnoremap("<leader>mu", [[<cmd>silent !kittyOneShot maketerm "~/.config/nvim/filetype/julia/docTest"<cr>]], "Run Doctests")

nnoremap("<leader>dd", "<cmd>silent !kittyPersistent debugterm juliadebug<cr>", "Open Debug Terminal")
nnoremap("<leader>de", [["<cmd>silent !kittyPersistent debugterm juliadebug '@enter . input("Debug? > ") . <cr>"]], "Enter Function")
nnoremap("<leader>dE", [["<cmd>silent !kittyPersistent debugterm juliadebug '@enter . getline(".") . <cr>"]], "Enter Function (Line)")
nnoremap("<leader>dn", "<cmd>silent !kittyPersistent debugterm juliadebug n<cr>", "Step to the Next Line")
nnoremap("<leader>dN", "<cmd>silent !kittyPersistent debugterm juliadebug c<cr>", "Step to the Next Breakpoint")
nnoremap("<leader>ds", "<cmd>silent !kittyPersistent debugterm juliadebug s<cr>", "Step In")
nnoremap("<leader>dS", "<cmd>silent !kittyPersistent debugterm juliadebug so<cr>", "Step Out")
nnoremap("<leader>dt", "<cmd>silent !kittyPersistent debugterm juliadebug bt<cr>", "Backtrace")
nnoremap("<leader>dv", "<cmd>silent !kittyPersistent debugterm juliadebug fr<cr>", "Variables")
nnoremap("<leader>dl", "<cmd>silent !kittyPersistent debugterm juliadebug st<cr>", "Status")
nnoremap("<leader>dw", "<cmd>silent !kittyPersistent debugterm juliadebug w<cr>", "Watchlist")
nnoremap("<leader>dc", "<cmd>silent !kittyPersistent debugterm juliadebug C<cr>", "Switch to Compiled Mode")
nnoremap("<leader>dW", [["<cmd>silent !kittyPersistent debugterm juliadebug 'w add " . input("Expression > ") . "'<cr>"]], "Add to Watchlist", expr)
nnoremap("<leader>dq", "<cmd>silent !kittyPersistent debugterm juliadebug q<cr>", "Quit")
nnoremap("<leader>di", "m1O@bp<esc>`1", "Insert Breakpoint Macro")
nnoremap("<leader>do", "<cmd>silent !kittyPersistent debugterm juliadebug o<cr>", "Jump to Line in Editor")
nnoremap("<leader>d+", "<cmd>silent !kittyPersistent debugterm juliadebug +<cr>", "Increase Lines of Source Code")
nnoremap("<leader>d-", "<cmd>silent !kittyPersistent debugterm juliadebug -<cr>", "Decrease Lines of Source Code")
nnoremap("<leader>db", [["<cmd>silent !kittyPersistent debugterm juliadebug 'bp add \"%:t\"\:" . line(".") . "'<cr>"]], "Set Breakpoint", expr)
nnoremap("<leader>dB", [["<cmd>silent !kittyPersistent debugterm juliadebug 'bp add \"%:t\"\:" . line(".") . " " . input("Condition > ") . "'<cr>"]], "Set Conditional Breakpoint", expr)
    mapxName.name("<leader>dr", "Breakpoints")
    nnoremap("<leader>drr", "<cmd>silent !kittyPersistent debugterm juliadebug bp<cr>", "List Breakpoints")
    nnoremap("<leader>drb", [["<cmd>silent !kittyPersistent debugterm juliadebug bp rm " . input("Point to Remove > ") . "<cr>"]], "Remove Breapoint")
    nnoremap("<leader>drw", [["<cmd>silent !kittyPersistent debugterm juliadebug w rm " . input("Item to Remove > ") . "<cr>"]], "Remove Watchlist")
nnoremap("<leader>/d", "<cmd>Edoc<cr>", "Documentation")
nnoremap("<leader>/D", "<cmd>EmainDoc<cr>", "Main Documentation")
nnoremap("<leader>/s", "<cmd>Esource<cr>", "Source")
nnoremap("<leader>/b", "<cmd>Ebench<cr>", "Benchmark")
nnoremap("<leader>/B", "<cmd>EmainBench<cr>", "Main Benchmark")
nnoremap("<leader>/t", "<cmd>Etest<cr>", "Test")
nnoremap("<leader>/T", "<cmd>EmainTest<cr>", "Main Test")
nnoremap("<leader>/p", "<cmd>Edeps<cr>", "Project Dependencies")
nnoremap("<leader>/S", [["<cmd>Esource " . split(getcwd(), '/')[-1] . "<cr>"]], "Main Source", expr)
    nnoremap("<leader>/nS", [["<cmd>Esource " . split(getcwd(), '/')[-1] . "<cr>"]], "Main Source", expr)
    nnoremap("<leader>/nd", [["<cmd>Edoc " . input('File Name > ') . "<cr>"]], "Documentation", expr)
    nnoremap("<leader>/nD", [[<cmd>EmainDoc<cr>]], "Main Documentation")
    nnoremap("<leader>/ns", [["<cmd>Esource " . input('File Name > ') . "<cr>"]], "Source", expr)
    nnoremap("<leader>/nb", [["<cmd>Ebench " . input('File Name > ') . "<cr>"]], "Benchmark", expr)
    nnoremap("<leader>/nB", [[<cmd>EmainBench<cr>]], "Main Benchmarks")
    nnoremap("<leader>/nt", [["<cmd>Etest " . input('File Name > ') . "<cr>"]], "Test", expr)
    nnoremap("<leader>/nT", [[<cmd>EmainTest<cr>]], "Main Tests")
    nnoremap("<leader>/np", [[<cmd>Edeps<cr>]], "Project Dependencies")
end)

    -- FIX : description text making things much slower, maybe move back to whichkey for filetype things

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
