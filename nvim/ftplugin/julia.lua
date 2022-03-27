vim.cmd([[set errorformat=%E%tRROR:%m]])
-- vim.cmd([[set errorformat+=%Zin\ expression\ starting\ at\ %f:%l]])
vim.cmd([[set errorformat+=%-G%.%#@\ /.%#]])
vim.cmd([[set errorformat+=%Z%.%#@\ %f:%l]])
vim.cmd([[set errorformat+=%C%.%#]])
vim.cmd([[set errorformat+=%-G%.%#]])
vim.api.nvim_set_option("makeprg", [[julia\ -e\ \'using\ Pkg;\ Pkg.precompile()\']])
vim.api.nvim_buf_set_option(0, "commentstring", [[#%s]])

vim.b[0].replCommand = "juliaREPL"
vim.b[0].replName = "JuliaREPL"
vim.b[0].debugCommand = "juliadebug"
vim.b[0].debugName = "JuliaDebug"

local handle = io.popen([[echo "$(basename "$PWD")"]])
local project = handle:read("*a")
handle:close()
vim.b[0].project = string.gsub(project, "\n", "")

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

nnoremap("<leader>mm", [[<cmd>silent !kittyOneShot maketerm "~/.config/nvim/filetype/julia/runExample"<cr>]], "Run Example Script")
nnoremap("<leader>mp", [[<cmd>silent !kittyOneShot maketerm "~/.config/nvim/filetype/julia/precompile"<cr>]], "Precompile")
nnoremap("<leader>mt", [[<cmd>silent !kittyOneShot maketerm "~/.config/nvim/filetype/julia/test"<cr>]], "Test Package")
nnoremap("<leader>mc", [[<cmd>silent !kittyOneShot maketerm "~/.config/nvim/filetype/julia/testCov"<cr>]], "Coverage Check Package")
nnoremap("<leader>mb", [[<cmd>silent !kittyOneShot maketerm "~/.config/nvim/filetype/julia/benchmark"<cr>]], "Benckmark Package")
nnoremap("<leader>md", [[<cmd>silent !kittyOneShot maketerm "~/.config/nvim/filetype/julia/docBuild"<cr>]], "Build Package Documentation")
nnoremap("<leader>mu", [[<cmd>silent !kittyOneShot maketerm "~/.config/nvim/filetype/julia/docTest"<cr>]], "Run Doctests")

nnoremap("<leader>jj", "<cmd>silent !kittyPersistent debugterm juliadebug<cr>", "Open Debug Terminal")
nnoremap("<leader>jq", "<cmd>silent !kitty @ close-window --match title:debugterm<cr>", "Close Debug Terms")
nnoremap("<leader>je", [["<cmd>silent !kittyPersistent debugterm juliadebug '@enter . input("Debug? > ") . <cr>"]], "Enter Function")
nnoremap("<leader>jE", [["<cmd>silent !kittyPersistent debugterm juliadebug '@enter . getline(".") . <cr>"]], "Enter Function (Line)")
nnoremap("<leader>jn", "<cmd>silent !kittyPersistent debugterm juliadebug n<cr>", "Step to the Next Line")
nnoremap("<leader>jN", "<cmd>silent !kittyPersistent debugterm juliadebug c<cr>", "Step to the Next Breakpoint")
nnoremap("<leader>js", "<cmd>silent !kittyPersistent debugterm juliadebug s<cr>", "Step In")
nnoremap("<leader>jS", "<cmd>silent !kittyPersistent debugterm juliadebug so<cr>", "Step Out")
nnoremap("<leader>jt", "<cmd>silent !kittyPersistent debugterm juliadebug bt<cr>", "Backtrace")
nnoremap("<leader>jv", "<cmd>silent !kittyPersistent debugterm juliadebug fr<cr>", "Variables")
nnoremap("<leader>jl", "<cmd>silent !kittyPersistent debugterm juliadebug st<cr>", "Status")
nnoremap("<leader>jw", "<cmd>silent !kittyPersistent debugterm juliadebug w<cr>", "Watchlist")
nnoremap("<leader>jc", "<cmd>silent !kittyPersistent debugterm juliadebug C<cr>", "Switch to Compiled Mode")
nnoremap("<leader>jW", [["<cmd>silent !kittyPersistent debugterm juliadebug 'w add " . input("Expression > ") . "'<cr>"]], "Add to Watchlist", expr)
nnoremap("<leader>jq", "<cmd>silent !kittyPersistent debugterm juliadebug q<cr>", "Quit")
nnoremap("<leader>ji", "m1O@bp<esc>`1", "Insert Breakpoint Macro")
nnoremap("<leader>jo", "<cmd>silent !kittyPersistent debugterm juliadebug o<cr>", "Jump to Line in Editor")
nnoremap("<leader>j+", "<cmd>silent !kittyPersistent debugterm juliadebug +<cr>", "Increase Lines of Source Code")
nnoremap("<leader>j-", "<cmd>silent !kittyPersistent debugterm juliadebug -<cr>", "Decrease Lines of Source Code")
nnoremap("<leader>jb", [["<cmd>silent !kittyPersistent debugterm juliadebug 'bp add \"%:t\"\:" . line(".") . "'<cr>"]], "Set Breakpoint", expr)
nnoremap("<leader>jB", [["<cmd>silent !kittyPersistent debugterm juliadebug 'bp add \"%:t\"\:" . line(".") . " " . input("Condition > ") . "'<cr>"]], "Set Conditional Breakpoint", expr)
    mapxName.name("<leader>jr", "Breakpoints")
    nnoremap("<leader>jrr", "<cmd>silent !kittyPersistent debugterm juliadebug bp<cr>", "List Breakpoints")
    nnoremap("<leader>jrb", [["<cmd>silent !kittyPersistent debugterm juliadebug bp rm " . input("Point to Remove > ") . "<cr>"]], "Remove Breapoint")
    nnoremap("<leader>jrw", [["<cmd>silent !kittyPersistent debugterm juliadebug w rm " . input("Item to Remove > ") . "<cr>"]], "Remove Watchlist")

    mapxName.name("<leader>lr", "Run Profiler")
    nnoremap("<leader>lrf", [["<cmd>silent !kittyOneShot maketerm julia ~/.config/nvim/filetype/julia/prof.jl '" . expand('%:p') . "'<cr>"]], "Profile File", expr)
    nnoremap("<leader>loo", function() require("perfanno").load_traces(jul_perf_flat("/tmp/julprof.data")) end, "Load Julia profile data")

nnoremap("<leader>/d", "<cmd>Edoc<cr>", "Documentation")
nnoremap("<leader>/D", "<cmd>EmainDoc<cr>", "Main Documentation")
nnoremap("<leader>/s", "<cmd>Esource<cr>", "Source")
nnoremap("<leader>/b", "<cmd>Ebench<cr>", "Benchmark")
nnoremap("<leader>/B", "<cmd>EmainBench<cr>", "Main Benchmark")
nnoremap("<leader>/t", "<cmd>Etest<cr>", "Test")
nnoremap("<leader>/T", [["<cmd>e test/" . b:project . "Tests.jl<cr>"]], "Main Test", expr)
nnoremap("<leader>/p", "<cmd>Edeps<cr>", "Project Dependencies")
nnoremap("<leader>/S", [["<cmd>Esource " . b:project . "<cr>"]], "Main Source", expr)
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
        ["benchmark/benchmarks.jl"] = { type = "mainBench" },
        ["docs/src/index.md"] = { type = "mainDoc" },
    }
}

local function get_command_output(cmd, silent)
    if silent then
        cmd = cmd .. " 2>/dev/null"
    end

    local data_file = assert(io.popen(cmd, "r"))
    data_file:flush()
    local output = data_file:read("*all")
    data_file:close()

    return output
end
function _G.jul_perf_flat(perf_data)
    local esc = vim.fn.fnameescape(perf_data)
    local raw_data = get_command_output("cat " .. perf_data, true)

    local result = {}
    local current_event = 1
    result[1] = {}

    for line in raw_data:gmatch("[^\r\n]+") do
        local count, file, linenr, symbol =
        line:match("^%s*(%d+)%s+%d+%s+(.-)%s+(%d+)%s+(.*)")
        local success = count and file and linenr and symbol

        if success and tonumber(count) > 0 then
            local trace = {symbol = symbol, file = file, linenr = tonumber(linenr)}

            table.insert(result[current_event], {count = tonumber(count), frames = {trace}})
        end
    end

    return result
end

-- Test selector function
vim.g.lastRun = "precompile"

local function runnable(selection)
    if not selection then
        return
    end
    vim.g.lastRun = selection

    vim.cmd([[silent !kittyPersistent testterm juliaTest ']] .. vim.b[0].project .. [[Tests.runtests("]] .. selection .. [[")']])
end
local function select_runnables()
    local handle = io.popen([[rg --no-filename --no-heading --no-line-number -e "^\s*@testset\s*\"(.*)\"\s*begin.*\$" -r "\$1"]])
    local tests = handle:read("*a")
    handle:close()
    local test_list = {}
    for s in tests:gmatch("([^\r\n]+)") do
        table.insert(test_list, s)
    end
    vim.ui.select(test_list, { prompt = "Select Test" }, runnable)
end

local function debuggable(selection)
    if not selection then
        return
    end
    vim.g.lastRun = selection

    vim.cmd([[silent !kittyPersistent testterm juliaTest '@enter ]] .. vim.b[0].project .. [[Tests.runtests("]] .. selection .. [[")']])
end
local function select_debuggables()
    local handle = io.popen([[rg --no-filename --no-heading --no-line-number -e "^\s*@testset\s*\"(.*)\"\s*begin.*\$" -r "\$1"]])
    local tests = handle:read("*a")
    handle:close()
    local test_list = {}
    for s in tests:gmatch("([^\r\n]+)") do
        table.insert(test_list, s)
    end
    vim.ui.select(test_list, { prompt = "Select Test" }, debuggable)
end

nnoremap("<leader>mm", function() select_runnables() end, buffer)
nnoremap("<leader>M", function() runnable(vim.g.lastRun) end, buffer)
nnoremap("<leader>jj", function() select_debuggables() end, buffer)
nnoremap("<leader>J", function() debuggable(vim.g.lastRun) end, buffer)
