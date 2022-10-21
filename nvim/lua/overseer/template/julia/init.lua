local constants = require("overseer.constants")
local files = require("overseer.files")
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

local julReplNum = 0

return {
    condition = {
        callback = function(opts)
            return isInProject(opts) or vim.bo.filetype == "julia"
        end
    },

    generator = function(_, cb)
        local otherProject = otherProjectFinder() or ""
        local otherProjectName = vim.fs.basename(otherProject) or ""
        local ret = {}
        local priority = 60
        local pr = function() priority = priority + 1; return priority end

        table.insert(
            ret,
            {
                name = "Open Julia Repl",
                builder = function()
                    julReplNum = julReplNum + 1
                    return {
                        name = "Julia Repl " .. julReplNum,
                        cmd = "julia --threads=auto",
                    }
                end,
                priority = pr(),
            }
        )
        table.insert(
            ret,
            {
                name = "Open Julia Repl in Project",
                builder = function()
                    julReplNum = julReplNum + 1
                    return {
                        name = vim.g.project .. " Project Repl " .. julReplNum,
                        cmd = "julia --threads=auto --project",
                    }
                end,
                condition = isProject,
                priority = pr(),
            }
        )
        table.insert(
            ret,
            {
                name = "Open Julia Repl in " .. otherProjectName .. " Project",
                builder = function()
                    julReplNum = julReplNum + 1
                    return {
                        name = otherProjectName .. " Project Repl " .. julReplNum,
                        cmd = "julia --threads=auto --project=" .. otherProject,
                    }
                end,
                condition = {
                    callback = function() return otherProjectName ~= vim.g.project and
                            otherProjectName ~= "."
                    end,
                },
                priority = pr(),
            }
        )

        local commands = {
            {
                name = "Start Test Server",
                tskName = vim.g.project .. " Test Server",
                cmd = [[julia --color=yes --project -t auto -e 'using Revise, DaemonMode; print("Running test server"); serve(print_stack=true, async=false)']],
                condition = isProject,
                is_test_server = true,
                components = { "default_hide", "unique", "always_restart" },
            },
            {
                name = "Build Documentation",
                tskName = vim.g.project .. " Doc Build",
                cmd = "~/.config/nvim/filetype/julia/docBuild",
                condition = { callback = isInProject },
                components = { "default", "unique" },
            },
            {
                name = "Open Built Documentation",
                cmd = "browser " .. vim.fn.expand("%:p:h") .. "/docs/build/index.html & sleep 5",
                condition = { callback = function(opts) files.exists(files.join(opts.dir, "docs", "build", "index.html")) end },
                components = { "default_hide", "unique" },
            },
            {
                name = "Start documentation Server",
                tskName = vim.g.project .. " Doc Server",
                cmd = [[julia --project=docs -E 'using Revise, ]] ..
                    vim.g.project .. [[, LiveServer; servedocs(launch_browser=true; include_dirs = ["src"])']],
                components = { "default_hide", "unique", "always_restart" },
                condition = hasDocs,
            },
            {
                name = "Open Documentation Server",
                cmd = "browser http://localhost:8000 & sleep 5",
                condition = hasDocs,
                components = { "default_hide", "unique" },
            },
            {
                name = "Run Documentation Tests",
                tskName = vim.g.project .. " Doc Test",
                cmd = "~/.config/nvim/filetype/julia/docTest",
                tags = { TAG.TEST },
                condition = hasDocs,
                components = { "default", "unique" },
            },
            {
                name = "Update Documentation Tests Output",
                tskName = vim.g.project .. " Doc Test Update",
                cmd = "~/.config/nvim/filetype/julia/docTestUpdate",
                condition = hasDocs,
                components = { "default", "unique" },
            },
            {
                name = "Test Package",
                tskName = vim.g.project .. " Test Suite",
                cmd = "julia --threads=auto --project -E 'using Pkg; Pkg.test()'",
                tags = { TAG.TEST },
                condition = hasTest,
                components = { "default", "unique" },
            },
            {
                name = "Test Coverage",
                tskName = vim.g.project .. " Test Coverage",
                cmd = "julia --threads=auto --project ~/.config/nvim/filetype/julia/task_test.jl " ..
                    vim.fs.basename(vim.fn.getcwd()),
                tags = { TAG.TEST },
                condition = hasTest,
                components = { "default", "unique" },
            },
            {
                name = "Package Benchmarks",
                tskName = vim.g.project .. " Bench Suite",
                cmd = [[julia -E 'using PkgBenchmark; benchmarkpkg("]] .. vim.g.project .. [[")']],
                condition = hasBenchmark,
                components = { "default", "unique" },
            },
            {
                name = "Retune Benchmarks",
                tskName = vim.g.project .. " Retune Bench",
                cmd = [[julia -E 'using PkgBenchmark; benchmarkpkg("]] .. vim.g.project .. [[, retune=true")']],
                condition = hasBenchmark,
                components = { "default", "unique" },
            },
            {
                name = "Run Julia File (" .. vim.fn.expand("%:t:r") .. ")",
                tskName = "Running " .. vim.fn.expand("%:t:r"),
                cmd = "julia " .. vim.fn.expand("%:p"),
                condition = isFile,
                components = { "default", "unique" },
            },
            {
                name = "Profile Package Imports",
                tskName = vim.g.project .. " Profile Imports",
                cmd = [[julia -E 'using InteractiveUtils; @time_imports using ]] .. vim.g.project .. "'",
                condition = isProject,
                components = { "default", "unique" },
            },
            {
                name = "Profile File (" .. vim.fn.expand("%:t:r") .. ")",
                tskName = "Profiling " .. vim.fn.expand("%:t:r"),
                cmd = "julia -i ~/.config/nvim/filetype/julia/prof.jl " .. vim.fn.expand("%:p"),
                condition = isFile,
                components = { "default", "unique" },
            },
            {
                name = "Run Build",
                tskName = vim.g.project .. " Build",
                cmd = "julia --threads=auto --project -E 'using Pkg; Pkg.build(" .. vim.g.project .. ")'",
                tags = { TAG.BUILD },
                condition = hasBuild,
                components = { "default", "unique" },
            },
            {
                name = "Precompile Package",
                cmd = "~/.config/nvim/filetype/julia/precompile",
                tskName = vim.g.project .. " Precompile",
                tags = { TAG.BUILD },
                condition = isProject,
                components = { "default", "unique" },
            },
            {
                name = "Package Compile",
                tskName = vim.g.project .. " Compile",
                cmd = "julia --threads=auto ~/.config/nvim/filetype/julia/task_compileenv.jl " .. vim.fn.getcwd(),
                tags = { TAG.BUILD },
                condition = isProject,
                components = { "default", "unique" },
            },
        }
        for _, command in pairs(commands) do
            table.insert(
                ret,
                {
                    name = command.name,
                    builder = function()
                        return {
                            name = command.tskName or command.name,
                            cmd = command.cmd,
                            components = command.components,
                            metadata = {
                                is_test_server = command.is_test_server,
                            },
                        }
                    end,
                    tags = command.tags,
                    priority = pr(),
                    params = {},
                    condition = command.condition,
                }
            )
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
                        name = "Test " .. san_name,
                        builder = function()
                            require("neotest").run.run(location)
                            return { cmd = "", name = "", components = { "user.dispose_now" }, }
                        end,
                        priority = pr(),
                        params = {},
                    }
                )
            end
        end

        -- FIX: This doesn't actually work
        for _, result in pairs(test_results) do
            result = vim.fn.json_decode(result)
            if result.type == "match" then
                local name = result.data.submatches[1].match.text
                local san_name = name:sub(2, -2)
                table.insert(
                    ret,
                    {
                        name = "Infiltrate " .. san_name,
                        builder = function()
                            return {
                                name = san_name .. " Infiltration",
                                cmd = [[julia --threads=auto --project -i -E '
                                using TestItemRunner, Infiltrator
                                @run_package_tests filter=ti->(ti.name == ]] .. name .. [[)
                                ']],
                                components = { "default", "unique" },
                            }
                        end,
                        priority = pr(),
                        params = {},
                    }
                )
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
                        name = "Benchmark " .. san_name,
                        builder = function()
                            require("neotest").run.run(location)
                            return { cmd = "", name = "", components = { "user.dispose_now" }, }
                        end,
                        priority = pr(),
                        params = {},
                    }
                )
            end
        end

        local profilable = vim.fn.systemlist([[rg --no-filename --no-heading --no-line-number -e ".*\[\"(.*?)\"\].*@benchmarkable(.*)\$" -r "\$1	\$2"]])
        for _, s in pairs(profilable) do
            local name, command = s:match("([^\t]+)\t([^\t]+)")
            table.insert(
                ret,
                {
                    name = "Profile " .. name,
                    builder = function()
                        return {
                            name = name .. " profiling",
                            cmd = "~/.config/nvim/filetype/julia/profBench '" .. command .. "'",
                            components = { "default", "unique" },
                        }
                    end,
                    priority = pr(),
                    params = {},
                }
            )
        end

        table.insert(
            ret,
            {
                name = "Load profile data",
                builder = function()
                    Jul_perf_flat()
                    return { cmd = "", name = "", components = { "user.dispose_now" }, }
                end,
                priority = pr(),
                condition = {
                    callback = function()
                        return files.exists("/tmp/julprof.data")
                    end
                },
                params = {},
            }
        )

        table.insert(
            ret,
            {
                name = "Benchmark against commit",
                builder = function()
                    local open_dif = function()
                        local action_state = require("telescope.actions.state")
                        local selected_entry = action_state.get_selected_entry()
                        local value = selected_entry["value"]
                        -- close Telescope window properly prior to switching windows
                        vim.api.nvim_win_close(0, true)
                        local task = require("overseer").new_task({
                            name = "Benchmark against " .. value,
                            cmd = [[julia -E 'using PkgBenchmark; res = judge("]] ..
                                vim.g.project .. '", "' .. value .. [[")
                                export_markdown("judgement.md", res)
                                ']],
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
                priority = pr(),
                condition = hasBenchmark,
            }
        )

        cb(ret)
    end,
}
