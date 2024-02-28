local constants = require("overseer.constants")
local files = require("overseer.files")
local TAG = constants.TAG

local isInProject = function(opts) return files.exists(files.join(vim.fn.getcwd(), "Project.toml")) end
local isFile = { filetype = "julia" }
local isProject = { callback = isInProject }
local hasTest = {
    callback = function(opts) return isInProject(opts) and files.exists(files.join(vim.fn.getcwd(), "test")) end }
local hasBenchmark = {
    callback = function(opts) return isInProject(opts) and files.exists(files.join(vim.fn.getcwd(), "benchmark")) end }
local hasDocs = {
    callback = function(opts) return isInProject(opts) and files.exists(files.join(vim.fn.getcwd(), "docs")) end }
local hasBuild = {
    callback = function(opts) return isInProject(opts) and files.exists(files.join(vim.fn.getcwd(), "build")) end }

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
        local juliaCommand = "julia --threads=auto "
        local otherProject = otherProjectFinder() or ""
        local otherProjectName = vim.fs.basename(otherProject) or ""
        local ret = {}
        local priority = 60
        local pr = function()
            priority = priority + 1; return priority
        end

        if files.exists(files.join(vim.fn.getcwd(), "JuliaSysimage.so")) then
            juliaCommand = juliaCommand .. "--sysimage JuliaSysimage.so "
        end

        table.insert(ret, {
            name = "Start Test Server",
            builder = function()
                return {
                    name = vim.g.project .. " Test Server",
                    cmd = juliaCommand ..
                        [[--project -e 'using Revise, DaemonMode; print("Running test server"); serve(print_stack=true)']],
                    components = { "default", "unique", "always_restart" },
                    metadata = { run_on_open = true },
                }
            end,
            condition = isProject,
            priority = pr(),
        }
        )
        table.insert(ret, {
            name = "Start documentation Server",
            builder = function()
                return {
                    name = vim.g.project .. " Doc Server",
                    cmd = juliaCommand .. [[--project=docs -E 'using Revise, ]] ..
                        vim.g.project .. [[, LiveServer; servedocs(
                            launch_browser=true;
                            literate=joinpath("docs","lit"),
                            include_dirs = ["src", "data"],
                        )']],
                    components = { "default", "unique", "always_restart" },
                    metadata = { run_on_open = true },
                }
            end,
            condition = hasDocs,
            priority = pr(),
        }
        )
        table.insert(ret, {
            name = "Build PDF Documentation",
            builder = function()
                return {
                    name = vim.g.project .. " Doc Build",
                    cmd = "~/.config/nvim/filetype/julia/docBuild",
                    components = { "default", "unique" },
                    env = { LATEX_DOCS = "true" },
                }
            end,
            condition = { callback = isInProject },
        })

        local commands = {
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
                condition = {
                    callback = function(opts) files.exists(files.join(opts.dir, "docs", "build", "index.html")) end },
                components = { "default", "unique" },
            },
            {
                name = "Open Documentation Server",
                cmd = "browser http://localhost:8000 & sleep 5",
                condition = hasDocs,
                components = { "default", "unique" },
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
                name = "Format " .. vim.g.project,
                tskName = vim.g.project .. " Formatting",
                cmd = juliaCommand ..
                    [[ -e 'using JuliaFormatter, ]] .. vim.g.project .. [[; format(]] ..
                    vim.g.project .. [[, format_markdown=true, verbose=true)']],
                components = { "default", "unique" },
            },
            {
                name = "Test Package",
                tskName = vim.g.project .. " Test Suite",
                cmd = juliaCommand .. "--project test/runtests.jl",
                tags = { TAG.TEST },
                condition = hasTest,
                components = { "default", "unique" },
            },
            {
                name = "Test Coverage",
                tskName = vim.g.project .. " Test Coverage",
                cmd = juliaCommand .. "--project --code-coverage=user test/runtests.jl",
                tags = { TAG.TEST },
                condition = hasTest,
                components = { "default", "unique" },
            },
            {
                name = "Package Benchmarks",
                tskName = vim.g.project .. " Bench Suite",
                cmd = juliaCommand .. [[-e 'using PkgBenchmark; benchmarkpkg("]] .. vim.g.project .. [[")']],
                condition = hasBenchmark,
                components = { "default", "unique" },
            },
            {
                name = "Retune Benchmarks",
                tskName = vim.g.project .. " Retune Bench",
                cmd = juliaCommand .. [[-e 'using PkgBenchmark; benchmarkpkg("]] .. vim.g.project .. [[, retune=true")']],
                condition = hasBenchmark,
                components = { "default", "unique" },
            },
            {
                name = "Profile Package Imports",
                tskName = vim.g.project .. " Profile Imports",
                cmd = [[julia -e 'using InteractiveUtils; @time_imports using ]] .. vim.g.project .. "'",
                condition = isProject,
                components = { "default", "unique" },
            },
            {
                name = "Run Build",
                tskName = vim.g.project .. " Build",
                cmd = juliaCommand .. "--project -e 'using Pkg; Pkg.build(" .. vim.g.project .. ")'",
                tags = { TAG.BUILD },
                condition = hasBuild,
                components = { "default", "unique" },
            },
            {
                name = "Precompile Package",
                cmd = juliaCommand .. "--project -e 'using Pkg; Pkg.precompile()'",
                tskName = vim.g.project .. " Precompile",
                tags = { TAG.BUILD },
                condition = isProject,
                components = { "default", "unique" },
            },
            {
                name = "Package Compile",
                tskName = vim.g.project .. " Compile",
                cmd = "julia --threads=auto --project ~/.config/nvim/filetype/julia/task_compileenv.jl " ..
                    vim.fn.getcwd(),
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
                        }
                    end,
                    tags = command.tags,
                    priority = pr(),
                    params = {},
                    condition = command.condition,
                }
            )
        end
        cb(ret)
    end,
}
