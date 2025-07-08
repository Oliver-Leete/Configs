---@module "overseer"
---@class OverseerUserConfig: overseer.Config
---@field extra_templates? table<string, (overseer.TemplateProvider|overseer.TemplateDefinition )> Allows to add extra templates or template providers elsewhere in the config

---@module "lazy"
---@type LazySpec
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
            "OverseerRestartLast",
            "OverseerInfo",
            "OverseerBuild",
            "OverseerQuickAction",
            "OverseerTaskAction",
            "OverseerClearCache",
        },
        keys = {
            { "<leader>nn", "<cmd>OverseerRun<cr>", desc = "Run task" },
            { "<leader>nN", "<cmd>OverseerRestartLast<cr>", desc = "Restart task" },
            { "<leader>no", "<cmd>OverseerToggle<cr>", desc = "Task list" },
            { "<leader>nb", "<cmd>OverseerBuild<cr>", desc = "Task builder" },
            { "<leader>nq", "<cmd>OverseerQuickAction<cr>", desc = "Action recent task" },
            { "<leader>nt", "<cmd>OverseerTaskAction<cr>", desc = "Task action" },
            { "<leader>nc", "<cmd>OverseerClearCache<cr>", desc = "Clear cache" },
            { "<leader>nl", "<cmd>OverseerLoadBundle<cr>", desc = "Load bundle" },
            { "<leader>ns", "<cmd>OverseerSaveBundle<cr>", desc = "Save bundle" },
            { "?N", "<cmd>OverseerInfo<cr>", desc = "Overseer info" },
        },
        ---@type OverseerUserConfig
        opts = {
            strategy = { "jobstart", preserve_output = true },
            dap = true,
            task_list = {
                direction = "left",
            },
            default_template_prompt = "allow",
            component_aliases = {
                default = {
                    { "display_duration", detail_level = 2 },
                    "on_output_summarize",
                    "on_exit_set_status",
                    { "on_complete_notify", system = "unfocused" },
                    { "on_complete_dispose", require_view = { "SUCCESS", "FAILURE" } },
                },
                unique_replace = { "unique", replace = false },
            },
        },
        opts_extend = { "extra_templates" },

        ---@param opts OverseerUserConfig
        config = function(_, opts)
            local extra_templates = opts.extra_templates or {}
            opts.extra_templates = nil

            local overseer = require("overseer")
            overseer.setup(opts)

            -- Register extra templates
            for _, template in pairs(extra_templates) do
                overseer.register_template(template)
            end

            -- Add a restart task command
            vim.api.nvim_create_user_command("OverseerRestartLast", function()
                local tasks = overseer.list_tasks({ recent_first = true })
                if vim.tbl_isempty(tasks) then
                    vim.notify("No tasks found", vim.log.levels.WARN)
                else
                    overseer.run_action(tasks[1], "restart")
                end
            end, {})
        end,
    },
}
