---@module "lazy"
---@type LazySpec
return {
    {
        "neovim/nvim-lspconfig",
        ---@module "plugins.lsp"
        ---@type PluginLspOpts
        opts = {
            servers = {
                ltex_plus = {
                    mason = true,
                    settings = {
                        ltex = {
                            language = "en-GB",
                            diagnosticSeverity = { MORFOLOGIK_RULE_EN_GB = "hint", default = "info" },
                            additionalRules = {
                                enablePickyRules = false,
                                motherTongue = "en-GB",
                            },
                            disabledRules = { ["en-GB"] = { "OXFORD_SPELLING_Z_NOT_S" } },
                        },
                    },
                },
            },
        },
    },
    {
        "barreiroleo/ltex_extra.nvim",
        dependencies = { "neovim/nvim-lspconfig" },
        init = function()
            vim.api.nvim_create_autocmd("LspAttach", {
                group = vim.api.nvim_create_augroup("UserLtexConfig", {}),
                callback = function(ev)
                    local client = vim.lsp.get_client_by_id(ev.data.client_id)
                    if client and client.name == "ltex_plus" then
                        require("ltex_extra").setup({
                            load_langs = { "en-GB" },
                            init_check = true,
                            path = ".ltex",
                            log_level = "none",
                        })
                    end
                end,
            })
        end,
    },
}
