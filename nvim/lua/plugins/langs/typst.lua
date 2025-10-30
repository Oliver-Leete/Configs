---@module "lazy"
---@type LazySpec
return {
    {
        "chomosuke/typst-preview.nvim",
        ft = "typst",
        opts = {},
        keys = {
            { "<localleader><localleader>", "<cmd>TypstPreviewToggle<cr>", desc = "Preview document", ft = "typst" },
        },
    },
    {
        "neovim/nvim-lspconfig",
        ---@module "plugins.lsp"
        ---@type PluginLspOpts
        opts = {
            servers = {
                tinymist = {
                    mason = false,
                    on_attach = function(client, bufnr)
                        print(bufnr)
                        vim.keymap.set(
                            "n",
                            "<localleader>tp",
                            function()
                                client:exec_cmd({
                                    title = "pin",
                                    command = "tinymist.pinMain",
                                    arguments = { vim.api.nvim_buf_get_name(0) },
                                }, { bufnr = bufnr })
                            end,
                            { desc = "[T]inymist [P]in", noremap = true }
                        )

                        vim.keymap.set(
                            "n",
                            "<localleader>tu",
                            function()
                                client:exec_cmd({
                                    title = "unpin",
                                    command = "tinymist.pinMain",
                                    arguments = { vim.v.null },
                                }, { bufnr = bufnr })
                            end,
                            { desc = "[T]inymist [U]npin", noremap = true }
                        )
                    end,
                },
            },
        },
    },
}
