return {
    {
        "neovim/nvim-lspconfig",
        opts = {
            servers = {
                ruff = {},
                basedpyright = {
                    analysis = {
                        useLibraryCodeForTypes = true,
                        diagnosticSeverityOverrides = {
                            diagnosticMode = "workspace",
                        },
                        diagnosticMode = "workspace",
                        typeCheckingMode = "basic",
                    },
                },

            },
        },
    },
}
