---@module "lazy"
---@type LazySpec
return {
    "artemave/workspace-diagnostics.nvim",
    opts = {},
    keys = {
        {
            "<leader>sd",
            function()
                for _, client in ipairs(vim.lsp.get_clients({ bufnr = 0 })) do
                    require("workspace-diagnostics").populate_workspace_diagnostics(client, 0)
                end
            end,
            desc = "Populate workspace diagnostics",
        },
    },
}
