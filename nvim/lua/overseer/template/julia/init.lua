local overseer = require("overseer")
local constants = require("overseer.constants")
local files = require("overseer.files")
local STATUS = require("overseer.constants").STATUS
local TAG = constants.TAG

local isInProject = function(opts) return files.exists(files.join(opts.dir, "Project.toml")) end
local isFile = { filetype = "julia" }
local isProject = { callback = isInProject }
local hasTest = { callback = function(opts) return isProject and files.exists(files.join(opts.dir, "test")) end }
local hasBenchmark = { callback = function(opts) return isProject and files.exists(files.join(opts.dir, "benchmark")) end }
local hasDocs = { callback = function(opts) return isProject and files.exists(files.join(opts.dir, "docs")) end }
local hasBuild = { callback = function(opts) return isProject and files.exists(files.join(opts.dir, "build")) end }

local otherProjectFinder = function()
    local projectDir
    for dir in vim.fs.parents(vim.api.nvim_buf_get_name(0)) do
        if files.exists(files.join(dir, "Project.toml")) then
            projectDir = dir
            break
        end
    end
    if projectDir then
        return projectDir
    end
end

return {
    condition = {
        callback = function(opts)
            return isInProject(opts) or vim.bo.filetype == "julia"
        end
    },

    generator = function(_, cb)

        local otherProject = otherProjectFinder()
        local otherProjectName = vim.fs.basename(otherProject)

        local commands = {
            {
                name = "Start Test Server",
                tskName = vim.g.project .. " Test Server",
                cmd = [[julia --color=yes --project -t auto -e 'using Revise, DaemonMode; print("Starting test server"); serve(print_stack=true, async=false)']],
                condition = isProject,
                is_test_server = true,
                hide = true,
                unique = true,
                alwaysRestart = true,
            },
            {
                name = "Open Julia Repl",
                tskName = "Julia Repl",
                cmd = "julia --threads=auto",
            },
            {
                name = "Open Julia Repl in Project",
                tskName = vim.g.project .. " Project Repl",
                cmd = "julia --threads=auto --project",
                condition = isProject,
            },
            {
                name = "Open Julia Repl in " .. otherProjectName .. " Project",
                tskName = otherProjectName .. " Project Repl",
                cmd = "julia --threads=auto --project=" .. otherProject,
                condition = {
                    callback = function() return otherProjectName ~= vim.g.project and otherProjectName ~= "." end,
                },
            },
            {
                name = "Build Documentation",
                tskName = vim.g.project .. " Doc Build",
                cmd = "~/.config/nvim/filetype/julia/docBuild",
                condition = { callback = isInProject },
                unique = true,
            },
            {
                name = "Open Built Documentation",
                cmd = "browser " .. vim.fn.expand("%:p:h") .. "/docs/build/index.html & sleep 5",
                condition = { callback = function(opts) files.exists(files.join(opts.dir, "docs", "build", "index.html")) end },
                hide = true,
                unique = true,
            },
            {
                name = "Start documentation Server",
                tskName = vim.g.project .. " Doc Server",
                cmd = [[julia --project=docs -e 'using Revise, ]] ..
                    vim.g.project .. [[, LiveServer; servedocs(launch_browser=true; include_dirs = ["src"])']],
                hide = true,
                unique = true,
                alwaysRestart = true,
                condition = hasDocs,
            },
            {
                name = "Open Documentation Server",
                cmd = "browser http://localhost:8000 & sleep 5",
                condition = hasDocs,
                hide = true,
                unique = true,
            },
            {
                name = "Run Documentation Tests",
                tskName = vim.g.project .. " Doc Test",
                cmd = "~/.config/nvim/filetype/julia/docTest",
                tags = { TAG.TEST },
                condition = hasDocs,
                unique = true,
            },
            {
                name = "Update Documentation Tests Output",
                tskName = vim.g.project .. " Doc Test Update",
                cmd = "~/.config/nvim/filetype/julia/docTestUpdate",
                condition = hasDocs,
                unique = true,
            },
            {
                name = "Test Package",
                tskName = vim.g.project .. " Test Suite",
                cmd = "cd test; julia --threads=auto --project runtests.jl",
                tags = { TAG.TEST },
                condition = hasTest,
                unique = true,
            },
            {
                name = "Test Coverage",
                tskName = vim.g.project .. " Test Coverage",
                cmd = "julia --threads=auto --project ~/.config/nvim/filetype/julia/task_test.jl " ..
                    vim.fs.basename(vim.fn.getcwd()),
                tags = { TAG.TEST },
                condition = hasTest,
                unique = true,
            },
            {
                name = "Package Benchmarks",
                tskName = vim.g.project .. " Bench Suite",
                cmd = [[julia -e 'using PkgBenchmark; benchmarkpkg("]] .. vim.g.project .. [[")']],
                condition = hasBenchmark,
                unique = true,
            },
            {
                name = "Retune Benchmarks",
                tskName = vim.g.project .. " Retune Bench",
                cmd = [[julia -e 'using PkgBenchmark; benchmarkpkg("]] .. vim.g.project .. [[, retune=true")']],
                condition = hasBenchmark,
                unique = true,
            },
            {
                name = "Run Julia File (" .. vim.fn.expand("%:t:r") .. ")",
                tskName = "Running " .. vim.fn.expand("%:t:r"),
                cmd = "julia " .. vim.fn.expand("%:p"),
                condition = isFile,
                unique = true,
            },
            {
                name = "Profile Package Imports",
                tskName = vim.g.project .. " Profile Imports",
                cmd = [[julia -e 'using InteractiveUtils; @time_imports using ]] .. vim.g.project .. "'",
                condition = isProject,
                unique = true,
            },
            {
                name = "Profile File (" .. vim.fn.expand("%:t:r") .. ")",
                tskName = "Profiling " .. vim.fn.expand("%:t:r"),
                cmd = "julia ~/.config/nvim/filetype/julia/prof.jl " .. vim.fn.expand("%:p"),
                condition = isFile,
                unique = true,
            },
            {
                name = "Run Build",
                tskName = vim.g.project .. " Build",
                cmd = "julia --threads=auto --project -e 'using Pkg; Pkg.build(" .. vim.g.project .. ")'",
                tags = { TAG.BUILD },
                condition = hasBuild,
                unique = true,
            },
            {
                name = "Precompile Package",
                cmd = "~/.config/nvim/filetype/julia/precompile",
                tskName = vim.g.project .. " Precompile",
                tags = { TAG.BUILD },
                condition = isProject,
                unique = true,
            },
            {
                name = "Package Compile",
                tskName = vim.g.project .. " Compile",
                cmd = "julia --threads=auto ~/.config/nvim/filetype/julia/task_compileenv.jl " .. vim.fn.getcwd(),
                tags = { TAG.BUILD },
                condition = isProject,
                unique = true,
            },
        }
        local ret = {}
        local priority = 60
        for _, command in pairs(commands) do

            local comps = {
                "on_output_summarize",
                "on_exit_set_status",
                { "on_complete_notify", system = "unfocused" },
                "on_complete_dispose",
            }
            if command.hide then
                table.insert(comps, { "user.attach_toggleterm", hide = true })
            else
                table.insert(comps, "user.attach_toggleterm")
            end
            if command.unique then
                table.insert(comps, "unique")
            end
            if command.alwaysRestart then
                table.insert(comps, { "on_complete_restart", statuses = { STATUS.FAILURE, STATUS.SUCCESS } })
            end

            table.insert(
                ret,
                {
                    name = command.name,
                    builder = function()
                        return {
                            name = command.tskName or command.name,
                            cmd = command.cmd,
                            components = comps,
                            metadata = {
                                is_test_server = command.is_test_server,
                            },
                        }
                    end,
                    tags = command.tags,
                    priority = priority,
                    params = {},
                    condition = command.condition,
                }
            )
            priority = priority + 1
        end

        -- Add neotest tests
        local test_results = vim.fn.systemlist([[rg --json -o --pcre2 '(?<=@testitem )".*"' $(pwd)]])
        for _, result in pairs(test_results) do
            result = vim.fn.json_decode(result)
            if result.type == "match" then
                local name = result.data.submatches[1].match.text
                local path = result.data.path.text
                local location = path .. "::" .. name
                local san_name = name:sub(2, -2)
                table.insert(
                    ret,
                    {
                        name = "Run " .. san_name .. " Test",
                        builder = function()
                            require("neotest").run.run(location)
                            return { cmd = "", name = "", components = { "user.dispose_now" }, }
                        end,
                        priority = priority,
                        params = {},
                    }
                )
                priority = priority + 1
            end
        end

        -- Add neotest benchmarks
        local benchmark_results = vim.fn.systemlist([[rg -o --json --pcre2 '".*"(?=] = @benchmarkable)' $(pwd)]])
        for _, result in pairs(benchmark_results) do
            result = vim.fn.json_decode(result)
            if result.type == "match" then
                local name = result.data.submatches[1].match.text
                local path = result.data.path.text
                local location = path .. "::" .. name
                local san_name = name:sub(2, -2)
                table.insert(
                    ret,
                    {
                        name = "Run " .. san_name .. " benchmark",
                        builder = function()
                            require("neotest").run.run(location)
                            return { cmd = "", name = "", components = { "user.dispose_now" }, }
                        end,
                        priority = priority,
                        params = {},
                    }
                )
                priority = priority + 1
            end
        end


        table.insert(
            ret,
            {
                name = "Julia benchmark against commit",
                builder = function()
                    local open_dif = function()
                        local action_state = require("telescope.actions.state")
                        local selected_entry = action_state.get_selected_entry()
                        local value = selected_entry["value"]
                        -- close Telescope window properly prior to switching windows
                        vim.api.nvim_win_close(0, true)
                        local task = require("overseer").new_task({
                            name = "Benchmark against " .. value,
                            cmd = [[julia -e 'using PkgBenchmark; judge("]] ..
                                vim.g.project .. '", "' .. value .. [[")']],
                            components = { "default", "unique" }
                        })
                        task:start()
                    end

                    local function git_commits_againsthead()
                        require("telescope.builtin").git_commits({
                            attach_mappings = function(_, map)
                                map("n", "<cr>", open_dif)
                                map("i", "<cr>", open_dif)
                                return true
                            end,
                        })
                    end

                    git_commits_againsthead()

                    return { cmd = "", name = "", components = { "user.dispose_now" }, }
                end,
                priority = priority,
                condition = hasBenchmark,
            }
        )
        priority = priority + 1

        local profilable = vim.fn.systemlist([[rg --no-filename --no-heading --no-line-number -e ".*\[\"(.*?)\"\].*@benchmarkable(.*)\$" -r "\$1	\$2"]])
        for _, s in pairs(profilable) do
            local name, command = s:match("([^\t]+)\t([^\t]+)")
            table.insert(
                ret,
                {
                    name = "Profile " .. name .. " Benchmark",
                    builder = function()
                        return {
                            name = name .. " profiling",
                            cmd = "~/.config/nvim/filetype/julia/profBench '" .. command .. "'",
                            components = { "default", "unique" },
                        }
                    end,
                    priority = priority,
                    params = {},
                }
            )
            priority = priority + 1
        end

        table.insert(
            ret,
            {
                name = "Load profile data",
                builder = function()
                    Jul_perf_flat()
                    return { cmd = "", name = "", components = { "user.dispose_now" }, }
                end,
                priority = priority,
                condition = {
                    callback = function()
                        return files.exists("/tmp/julprof.data")
                    end
                },
                params = {},
            }
        )

        cb(ret)
    end,
}
