vim.cmd([[set errorformat=%E%tRROR:%m]])
-- vim.cmd([[set errorformat+=%Zin\ expression\ starting\ at\ %f:%l]])
vim.cmd([[set errorformat+=%-G%.%#@\ /.%#]])
vim.cmd([[set errorformat+=%Z%.%#@\ %f:%l]])
vim.cmd([[set errorformat+=%C%.%#]])
vim.cmd([[set errorformat+=%-G%.%#]])
vim.api.nvim_set_option("makeprg", [[julia\ -e\ \'using\ Pkg;\ Pkg.precompile()\']])
vim.api.nvim_buf_set_option(0, "commentstring", [[#%s]])

vim.g.latex_to_unicode_tab = 0
vim.g.latex_to_unicode_auto = 1

vim.b[0].replCommand = "juliaREPL"
vim.b[0].replName = "JuliaPersistant"
vim.b[0].debugCommand = "juliadebug"
vim.b[0].debugName = "JuliaPersistant"

local handle = io.popen([[echo "$(basename "$PWD")"]])
local project = handle:read("*a")
handle:close()
vim.b[0].project = string.gsub(project, "\n", "")

Map("n", "<leader>/d", "<cmd>Edoc<cr>", { buffer = 0 })
Map("n", "<leader>/D", "<cmd>EmainDoc<cr>", { buffer = 0 })
Map("n", "<leader>/s", "<cmd>Esource<cr>", { buffer = 0 })
Map("n", "<leader>/t", "<cmd>Etest<cr>", { buffer = 0 })
Map("n", "<leader>/T", [["<cmd>e test/" . b:project . "Tests.jl<cr>"]], { expr = true, buffer = 0 })
Map("n", "<leader>/p", "<cmd>Edeps<cr>", { buffer = 0 })
Map("n", "<leader>/S", [["<cmd>Esource " . b:project . "<cr>"]], { expr = true, buffer = 0 })

vim.g.projectionist_heuristics = {
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
		["README.md"] = { type = "readme" },
		["Project.toml"] = { type = "deps" },
		["docs/src/index.md"] = { type = "mainDoc" },
	},
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
	-- local esc = vim.fn.fnameescape(perf_data)
	local raw_data = get_command_output("cat " .. perf_data, true)

	local result = {}
	local current_event = 1
	result[1] = {}

	for line in raw_data:gmatch("[^\r\n]+") do
		local count, file, linenr, symbol = line:match("^%s*(%d+)%s+%d+%s+(.-)%s+(%d+)%s+(.*)")
		local success = count and file and linenr and symbol

		if success and tonumber(count) > 0 then
			if file:find("@" .. vim.b[0].project) then
				file = "/home/oleete/Projects/" .. file.match(file, "@(" .. vim.b[0].project .. ".*)")
				local trace = { symbol = symbol, file = file, linenr = tonumber(linenr) }

				table.insert(result[current_event], { count = tonumber(count), frames = { trace } })
			end
		end
	end

	return result
end

vim.b[0].localCommands = {
	{ source = "julia", name = "Tasks", func = Select_runnables },
	{ source = "julia", name = "Fetch errors from persistant", command = "silent !kittyQuickfix juliaTest" },
	{ source = "julia", name = "Fetch errors from one shot", command = "silent !kittyQuickfix OneShot" },
	{
		source = "julia",
		name = "Load profile data",
		func = function()
			require("perfanno").load_traces(jul_perf_flat("/tmp/julprof.data"))
		end,
	},
}

-- Test selector function
vim.b[0].LastCommand = {
	name = "Precompile Package",
	type = "misc",
	label = "[Misc] ",
	command = function()
		vim.cmd([[silent !kittyOneShot "~/.config/nvim/filetype/julia/precompile"]])
	end,
}

function Select_runnables()
	-- Misc Runnables
	local runnables_list = {
		{
			source = "Misc",
			name = "Open Runnable Terminal",
			command = [[silent !kittyPersistent JuliaPersistant juliaTest]],
		},
		{
			source = "Misc",
			name = "Precompile Package",
			command = [[silent !kittyOneShot "~/.config/nvim/filetype/julia/precompile"]],
		},
		{
			source = "Misc",
			name = "Build Documentation",
			command = [[silent !kittyOneShot "~/.config/nvim/filetype/julia/docBuild"]],
		},
		{
			source = "Misc",
			name = "Run Documentation Tests",
			command = [[silent !kittyOneShot "~/.config/nvim/filetype/julia/docTest"]],
		},
		{
			source = "Test",
			name = "Run All Tests",
			command = [[silent !kittyPersistent JuliaPersistant juliaTest ']]
				.. vim.b[0].project
				.. [[Tests.runtests(;spin=false)']],
		},
		{
			source = "Test",
			name = "Run All Tests",
			command = [[silent !kittyPersistent JuliaPersistant juliaTest ']]
				.. vim.b[0].project
				.. [[Tests.runtests(;spin=false)']],
		},
		{
			source = "Bench",
			name = "Run All Benchmarks",
			command = [[silent !kittyPersistent JuliaPersistant juliaTest 'run(]]
				.. vim.b[0].project
				.. [[Tests.suite, verbose=true)']],
		},
		{
			source = "Bench",
			name = "Retune Benchmarks",
			command = [[silent !kittyPersistent JuliaPersistant juliaTest 'let suite=]]
				.. vim.b[0].project
				.. [[Tests.suite; tune\!(suite); BenchmarkTools.save(joinpath(dirname(@__FILE__), "params.json"), params(suite));end']],
		},
		{
			source = "Run",
			name = "Run File",
			command = [[silent !kittyOneShot julia ']] .. vim.fn.expand("%:p") .. "'",
		},
		{
			source = "Prof",
			name = "Profile File",
			command = [[silent !kittyOneShot julia ~/.config/nvim/filetype/julia/prof.jl ']]
				.. vim.fn.expand("%:p")
				.. "'",
		},
	}

	-- Tests
	local handle1 = io.popen(
		[[rg --no-filename --no-heading --no-line-number -e "^\s*@testitem\s*\"(.*)\"\s*begin.*\$" -r "\$1"]]
	)
	local tests = handle1:read("*a")
	handle1:close()

	for name in tests:gmatch("([^\r\n]+)") do
		table.insert(runnables_list, {
			source = "Test",
			name = name,
			command = [[silent !kittyPersistent JuliaPersistant juliaTest ']]
				.. vim.b[0].project
				.. [[Tests.runtests("]]
				.. name
				.. [[",spin=false)']],
		})
	end

	-- Benchmarks

	local handle2 = io.popen(
		[[rg --no-filename --no-heading --no-line-number -e ".*\[\"(.*?)\"\].*@benchmarkable(.*)\$" -r "\$1	\$2"]]
	)
	local benches = handle2:read("*a")
	handle2:close()

	for s in benches:gmatch("([^\r\n]+)") do
		local name, command = s:match("([^\t]+)\t([^\t]+)")
		table.insert(runnables_list, {
			source = "Bench",
			name = name,
			command = [[silent !kittyPersistent JuliaPersistant juliaTest 'run(]]
				.. vim.b[0].project
				.. [[Tests.suite["]]
				.. name
				.. [["], verbose=true)']],
		})
		table.insert(runnables_list, {
			source = "Prof",
			name = name,
			command = [[silent !kittyPersistent JuliaPersistant juliaTest 'a = @bprofile ]]
				.. command
				.. [[; Profile.print(IOContext(open("/tmp/julprof.data", "w"), :displaysize=>(100000,1000)), format=:flat); ProfileView.view(); loadProfData(); a']],
		})
		table.insert(runnables_list, {
			source = "Debug",
			name = name,
			command = [[silent !kittyPersistent JuliaPersistant juliaTest '@run run(]]
				.. vim.b[0].project
				.. [[Tests.suite["]]
				.. name
				.. [["], verbose=true)']],
		})
	end

	-- table.sort(runnables_list, function(a, b) return a.name < b.name end)
	-- Selection
	CommandCentre(runnables_list)
end

Map("n", "<leader>d", Select_runnables, { buffer = 0 })
-- Map("n", "<leader>D", function() runnable(vim.b[0].LastCommand) end, {buffer=0})
