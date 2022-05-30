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

-- Test selector function
vim.b[0].LastCommand = {
	name = "Precompile Package",
	type = "misc",
	label = "[Misc] ",
	command = function()
		vim.cmd([[silent !kittyOneShot "~/.config/nvim/filetype/julia/precompile"]])
	end,
}

vim.b[0].runnables = function()
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
			command = [[silent !kittyOneShot "julia ']] .. vim.fn.expand("%:p") .. [['"]],
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
				.. [[; Profile.print(IOContext(open("/tmp/julprof.data", "w"), :displaysize=>(100000,1000)), format=:flat); ]]
				.. [[ProfileView.view(); loadProfData(); a']],
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
	return runnables_list
end

Run_closest = function()
	local func_pat = "function [^%s]"
	local func_num = vim.fn.search(func_pat, "nbWc")

	local ass_pat = "[^%s](.*) = "
	local ass_num = vim.fn.search(ass_pat, "nbWc")

	local struct_pat = "struct [^%s]"
	local struct_num = vim.fn.search(struct_pat, "nbWc")

	local line_num = math.max(func_num, ass_num, struct_num)

	if line_num == 0 then
		return
	end

	local line = vim.fn.getline(line_num)
	local name
	if struct_num < math.max(func_num, ass_num) then
		name = line:match("([^%s]+)%(")
	else
		name = line:match("struct ([%w^_-]+)")
	end
	vim.cmd(
		[[silent !kittyPersistent JuliaPersistant juliaTest ']]
			.. vim.b[0].project
			.. [[Tests.runtests("]]
			.. name
			.. [[",spin=false)']]
	)
end

local bp_remover = function(imp_line, bp_pattern, mod_pat)
	vim.cmd("delete")

	-- if there are no bps left, remove the import
	local mod_1 = vim.fn.search(mod_pat, "nbW")
	local mod_2 = vim.fn.search(mod_pat, "ncW")

	local bp_1 = vim.fn.search(bp_pattern, "nbW")
	local bp_2 = vim.fn.search(bp_pattern, "ncW")

	if (bp_1 == 0 or (bp_1 < mod_1 and mod_1 ~= 0)) and (bp_2 == 0 or (bp_2 > mod_2 and mod_2 ~= 0)) then
		local imp_num = vim.fn.search(imp_line, "nbW")

		if imp_num < mod_1 then
			return
		end

		local cur_pos = vim.fn.getcurpos()
		vim.cmd(imp_num .. "delete")
		vim.fn.cursor(cur_pos[2] - 2, cur_pos[3])
	end
end

local bp_adder = function(bp, imp_line, mod_pat)
	local mod_num = vim.fn.search(mod_pat, "nbW")
	local imp_num = vim.fn.search(imp_line, "nbW")
	if imp_num == 0 or imp_num < mod_num then
		vim.fn.append(mod_num, imp_line)
	end

	vim.cmd("normal! o" .. bp)
end

BP_Toggle = function(imp_name, bp)
	local mod_pat = "^[%s]*module .*"

	local imp_line = "using " .. imp_name
	local bp_pattern = "^[%s]*" .. bp .. ".*"

	if string.match(vim.fn.getline("."), bp_pattern) then
		bp_remover(imp_line, bp_pattern, mod_pat)
	else
		bp_adder(bp, imp_line, mod_pat)
	end
end

BP_Remove_All = function(imp_names, bp_names)
	local cur_pos = vim.fn.getcurpos()
	for _, imp_name in pairs(imp_names) do
		local imp_line = "using " .. imp_name
		local imp_num = vim.fn.search(imp_line, "nw")

		while imp_num ~= 0 do
			vim.cmd(imp_num .. "delete")
			imp_num = vim.fn.search(imp_line, "nw")
		end
	end

	for _, bp_name in pairs(bp_names) do
		local bp_line = "[%s]*" .. bp_name .. ".*"
		local bp_num = vim.fn.search(bp_line, "nw")

		while bp_num ~= 0 do
			vim.cmd(bp_num .. "delete")
			bp_num = vim.fn.search(bp_line, "nw")
		end
	end
	vim.fn.cursor(cur_pos[2] - 2, cur_pos[3])
end

Map("n", "<leader>/d", "<cmd>Edoc<cr>", { buffer = 0 })
Map("n", "<leader>/D", "<cmd>EmainDoc<cr>", { buffer = 0 })
Map("n", "<leader>/s", "<cmd>Esource<cr>", { buffer = 0 })
Map("n", "<leader>/t", "<cmd>Etest<cr>", { buffer = 0 })
Map("n", "<leader>/T", [["<cmd>e test/" . b:project . "Tests.jl<cr>"]], { expr = true, buffer = 0 })
Map("n", "<leader>/p", "<cmd>Edeps<cr>", { buffer = 0 })
Map("n", "<leader>/S", [["<cmd>Esource " . b:project . "<cr>"]], { expr = true, buffer = 0 })

Map("n", ",rb", "<cmd>call julia#toggle_function_blockassign()<cr>")

Map("n", "<leader>D", Run_closest, { expr = true, buffer = 0 })

Map("n", ",dd", function()
	BP_Toggle("Debugger", "@bp")
end, { buffer = 0 })
Map("n", ",di", function()
	BP_Toggle("Infiltrator", "@infiltrate")
end, { buffer = 0 })
Map("n", ",dq", function()
	BP_Remove_All({ "Debugger", "Infiltrator" }, { "@bp", "@infiltrate" })
end, { buffer = 0 })

vim.b[0].localCommands = {
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
