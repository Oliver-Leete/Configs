---@module "lazy"
---@type LazySpec
return {
    {
        "folke/which-key.nvim",
        optional = true,
        opts = {
            to_add = {
                logging = {
                    { ",l", group = "Logging", icon = "󰹈 " },
                },
            },
        },
    },
    {
        "chrisgrieser/nvim-chainsaw",
        lazy = false,
        priority = 200,
        dependencies = {
            {"folke/snacks.nvim"},
        },
        opts = {
            marker = "󰹈 ",
            visuals = {
                icon = "󰹈 ",
            },
            logStatements = {
                variableLog = {
                    nvim_lua = "Chainsaw({{var}}) -- {{marker}}",
                    julia = [[println("{{marker}} {{var}}:", {{var}})]],
                },
                assertLog = {
                    julia = [[@assert {{var}} "{{marker}} {{var}}"]],
                },
                typeLog = {
                    julia = [[println("{{marker}} {{var}} is of type ", typeof({{var}}))]],
                },
                messageLog = {
                    julia = [[println("{{marker}} ")]],
                },
                stacktraceLog = {
                    julia = [[println("{{marker}} stacktrace: ", stacktrrace())]],
                },
                debugLog = {
                    julia = "@bp",
                },
                clearLog = {
                    julia = "Base.run(`clear`)",
                },
                timeLogStart = {
                    julia = [[timelog_start_{{index}} = time() # {{marker}}]],
                },
                timeLogStop = {
                    julia = [[println("#{{index}} {{marker}}: ", time() - timelog_start_{{index}})]],
                },
            },
        },
        keys = {
            { ",ll", function() require("chainsaw").messageLog() end, desc = "Log message" },
            { ",lv", function() require("chainsaw").variableLog() end, desc = "Log variable" },
            { ",lo", function() require("chainsaw").objectLog() end, desc = "Log object" },
            { ",lt", function() require("chainsaw").typeLog() end, desc = "Log type" },
            { ",la", function() require("chainsaw").assertLog() end, desc = "Log assertion" },
            { ",ld", function() require("chainsaw").timeLog() end, desc = "Log duration" },
            { ",lb", function() require("chainsaw").debugLog() end, desc = "Log breakpoint" },
            { ",ls", function() require("chainsaw").stacktraceLog() end, desc = "Log stack trace" },
            { ",lc", function() require("chainsaw").clearLog() end, desc = "Log clear" },
            { ",lr", function() require("chainsaw").removeLogs() end, desc = "Remove logs", mode = { "n", "x" } },
        },
    },
}
