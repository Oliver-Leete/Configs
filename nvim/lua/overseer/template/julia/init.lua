local overseer = require("overseer")
local constants = require("overseer.constants")
local files = require("overseer.files")
local TAG = constants.TAG

return {
    condition = {
        callback = function(opts)
            return files.exists(files.join(opts.dir, "Project.toml"))
        end
    },

    generator = function(_)
        local commands = {
            {
                name = "Package Precompile",
                cmd = "~/.config/nvim/filetype/julia/precompile",
                tags = { TAG.BUILD }
            },
            {
                name = "Build Documentation",
                cmd = "~/.config/nvim/filetype/julia/docBuild",
            },
            {
                name = "Open preBuilt Documentation",
                cmd = "browser " .. vim.fn.expand("%:p:h") .. "/docs/build/index.html & sleep 5",
            },
            {
                name = "Start Documentation Server",
                cmd = [[julia --project=docs -ie 'using ]] ..
                    vim.g.project .. [[, LiveServer; servedocs(launch_browser=true)']]
            },
            {
                name = "Open Live Documentation Server",
                cmd = "browser http://localhost:8000 & sleep 5",
            },
            {
                name = "Documentation Tests",
                cmd = "~/.config/nvim/filetype/julia/docTest",
                tags = { TAG.TEST }
            },
            {
                name = "Run all Tests",
                cmd = "~/.config/nvim/lua/neotest-julia-retest/juliaTestRunner",
            },
            {
                name = "Run all Benchmarks",
                cmd = "~/.config/nvim/lua/neotest-julia-benchmarktools/juliaBenchmarkRunner suite",
            },
            {
                name = "Retune Benchmarks",
                cmd = [[julia -e '
                using BenchmarkTools
                include("benchmark/PackageBenchmarks.jl")
                tune!(suite)
                BenchmarkTools.save(joinpath(dirname(@__FILE__), "params.json"), params(suite))
                ']],
            },
            {
                name = "Run File (" .. vim.fn.expand("%:t:r") .. ")",
                cmd = "julia " .. vim.fn.expand("%:p")
            },
            {
                name = "Profile Imports",
                cmd = [[julia +beta -e '@time_imports using ]] .. vim.g.project .. "'"
            },
            {
                name = "Profile File (" .. vim.fn.expand("%:t:r") .. ")",
                cmd = "julia ~/.config/nvim/filetype/julia/prof.jl " .. vim.fn.expand("%:p")
            },
        }
        local ret = {}
        local priority = 60
        for _, command in pairs(commands) do
            table.insert(
                ret,
                {
                    name = command.name,
                    builder = function()
                        return {
                            name = command.name,
                            cmd = command.cmd,
                        }
                    end,
                    tags = command.tags,
                    priority = priority,
                    params = {},
                }
            )
            priority = priority + 1
        end

        local handle2 = io.popen(
            [[rg --no-filename --no-heading --no-line-number -e ".*\[\"(.*?)\"\].*@benchmarkable(.*)\$" -r "\$1	\$2"]]
        )
        local benches
        if handle2 then
            benches = handle2:read("*a")
            handle2:close()

            for s in benches:gmatch("([^\r\n]+)") do
                local name, command = s:match("([^\t]+)\t([^\t]+)")
                table.insert(
                    ret,
                    {
                        name = "Profile " .. name,
                        builder = function()
                            return {
                                name = "Profile " .. name,
                                cmd = "~/.config/nvim/filetype/julia/profBench '" .. command .. "'",
                            }
                        end,
                        priority = priority + 1,
                        params = {},
                    }
                )
            end
        end
        return ret
    end,
}
