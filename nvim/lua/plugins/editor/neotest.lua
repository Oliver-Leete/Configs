---@module "lazy"
---@type LazySpec
return {
    {
        "nvim-neotest/neotest",
        cmd = { "Neotest" },
        opts = {
            -- Can be a list of adapters like what neotest expects,
            -- or a list of adapter names,
            -- or a table of adapter names, mapped to adapter configs.
            -- The adapter will then be automatically loaded with the config.
            adapters = {
                "neotest-haskell",
                "rustaceanvim.neotest",
                "neotest-python",
            },
            -- Example for loading neotest-golang with a custom config
            -- adapters = {
            --   ["neotest-golang"] = {
            --     go_test_args = { "-v", "-race", "-count=1", "-timeout=60s" },
            --     dap_go_enabled = true,
            --   },
            -- },
            status = { signs = false, virtual_text = true },
            output = { open_on_run = true },
            quickfix = {
                open = function() require("trouble").open({ mode = "quickfix", focus = false }) end,
            },
        },
        dependencies = {
            "nvim-neotest/nvim-nio",
            "mrcjkb/neotest-haskell",
            "nvim-neotest/neotest-python",
        },
        config = function(_, opts)
            local neotest_ns = vim.api.nvim_create_namespace("neotest")
            vim.diagnostic.config({
                virtual_text = {
                    format = function(diagnostic)
                        -- Replace newline and tab characters with space for more compact diagnostics
                        local message =
                            diagnostic.message:gsub("\n", " "):gsub("\t", " "):gsub("%s+", " "):gsub("^%s+", "")
                        return message
                    end,
                },
            }, neotest_ns)

            opts.consumers = opts.consumers or {}
            -- Refresh and auto close trouble after running tests
            ---@module "neotest"
            ---@type neotest.Consumer
            opts.consumers.trouble = function(client)
                client.listeners.results = function(adapter_id, results, partial)
                    if partial then return end
                    local tree = assert(client:get_position(nil, { adapter = adapter_id }))

                    local failed = 0
                    for pos_id, result in pairs(results) do
                        if result.status == "failed" and tree:get_key(pos_id) then failed = failed + 1 end
                    end
                    vim.schedule(function()
                        local trouble = require("trouble")
                        if trouble.is_open() then
                            trouble.refresh()
                            if failed == 0 then trouble.close() end
                        end
                    end)
                end
                return {}
            end

            if opts.adapters then
                local adapters = {}
                for name, config in pairs(opts.adapters or {}) do
                    if type(name) == "number" then
                        if type(config) == "string" then config = require(config) end
                        vim.list_extend(adapters, { config })
                    elseif config ~= false then
                        local adapter = require(name)
                        if type(config) == "table" and not vim.tbl_isempty(config) then
                            local meta = getmetatable(adapter)
                            if adapter.setup then
                                adapter.setup(config)
                            elseif adapter.adapter then
                                adapter.adapter(config)
                                adapter = adapter.adapter
                            elseif meta and meta.__call then
                                adapter = adapter(config)
                            else
                                error("Adapter " .. name .. " does not support setup")
                            end
                        end
                        vim.list_extend(adapters, { adapter })
                    end
                end
                opts.adapters = adapters
            end

            require("neotest").setup(opts)
        end,
        -- stylua: ignore
        keys = {
            { "<leader>uu", function() require("neotest").run.run() end,                                        desc = "Run Nearest" },
            { "<leader>uT", function() require("neotest").run.run({ strategy = "dap" }) end,                    desc = "Debug Nearest" }, ---@diagnostic disable-line: missing-fields
            { "<leader>ul", function() require("neotest").run.run_last() end,                                   desc = "Run Last" },
            { "<leader>ul", function() require("neotest").run.run_last({ strategy = "dap" }) end,               desc = "Debug Last" }, ---@diagnostic disable-line: missing-fields
            { "<leader>uf", function() require("neotest").run.run(vim.fn.expand("%")) end,                      desc = "Run File" },
            { "<leader>uF", function() require("neotest").run.run(vim.uv.cwd()) end,                            desc = "Run All Test Files" },
            { "<leader>us", function() require("neotest").run.stop() end,                                       desc = "Stop nearest" },

            { "<leader>us", function() require("neotest").summary.toggle() end,                                 desc = "Toggle Summary" },
            { "<leader>uo", function() require("neotest").output.open({ enter = true, auto_close = true }) end, desc = "Show Output" },
            { "<leader>uO", function() require("neotest").output_panel.toggle() end,                            desc = "Toggle Output Panel" },

            { "<leader>uw", function() require("neotest").watch.toggle(vim.fn.expand("%")) end,                 desc = "Toggle Watch" },
        },
    },
}
