return {
    {
        "neovim/nvim-lspconfig",
        opts = { servers = { contextive = {}, }, },
    },
    {
        "barreiroleo/ltex_extra.nvim",
        ft = { "markdown", "tex" },
        dependencies = { "neovim/nvim-lspconfig" },
        opts = {
            load_langs = { "en-GB" },
            init_check = true,
            path = ".ltex",
            log_level = "none",
            server_opts = {
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
        }
    }
}
