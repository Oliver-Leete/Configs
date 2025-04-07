return {
    {
        "stevearc/overseer.nvim",
        cmd = {
            "OverseerOpen",
            "OverseerClose",
            "OverseerToggle",
            "OverseerSaveBundle",
            "OverseerLoadBundle",
            "OverseerDeleteBundle",
            "OverseerRunCmd",
            "OverseerRun",
            "OverseerInfo",
            "OverseerBuild",
            "OverseerQuickAction",
            "OverseerTaskAction",
            "OverseerClearCache",
        },
        keys = {
            { "<leader>ow", "<cmd>OverseerToggle<cr>",      desc = "Task list" },
            { "<leader>oo", "<cmd>OverseerRun<cr>",         desc = "Run task" },
            { "<leader>oq", "<cmd>OverseerQuickAction<cr>", desc = "Action recent task" },
            { "<leader>oi", "<cmd>OverseerInfo<cr>",        desc = "Overseer Info" },
            { "<leader>ob", "<cmd>OverseerBuild<cr>",       desc = "Task builder" },
            { "<leader>ot", "<cmd>OverseerTaskAction<cr>",  desc = "Task action" },
            { "<leader>oc", "<cmd>OverseerClearCache<cr>",  desc = "Clear cache" },
        },
        opts = {
            strategy = "jobstart",
            dap = true,
            task_list = {
                direction = "left",
            },
        }
    },
    {
        "folke/edgy.nvim",
        optional = true,
        opts = function(_, opts)
            opts.left = opts.bottom or {}
            table.insert(opts.bottom, {
                title = "overseer-list",
                ft = "OverseerList",
                open = "OverseerOpen",
            })
        end,
    },
    {
        "nvim-neotest/neotest",
        optional = true,
        opts = function(_, opts)
            opts = opts or {}
            opts.consumers = opts.consumers or {}
            opts.consumers.overseer = require("neotest.consumers.overseer")
        end,
    },
}
