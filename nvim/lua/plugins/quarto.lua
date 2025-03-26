local quarto_setup = function()
    -- Image
    require("image").setup({
        backend = "kitty",
        integrations = {},
        max_width = 100,
        max_height = 12,
        max_height_window_percentage = math.huge,
        max_width_window_percentage = math.huge,
        window_overlap_clear_enabled = true,
        window_overlap_clear_ft_ignore = { "cmp_menu", "cmp_docs", "" },
    })

    -- Molten
    vim.g.python3_host_prog = vim.fn.expand("~/.local/python_venvs/molten-nvim/bin/python3")

    vim.g.molten_image_provider = "image.nvim"
    vim.g.molten_output_win_max_height = 20
    vim.g.molten_auto_image_popup = true
    vim.g.molten_auto_open_html_in_browser = true

    -- Otter
    require("otter").setup({
        lsp = {
            root_dir = function(_, bufnr)
                return vim.fs.root(bufnr or 0, {
                    ".git",
                    "_quarto.yml",
                    "package.json",
                }) or vim.fn.getcwd(0)
            end,
        },
    })

    -- Quarto
    local quarto = require('quarto')
    quarto.setup({
        codeRunner = {
            enabled = true,
            default_method = "molten",
        },
    })

    vim.keymap.set({ "n" }, "<leader>qq", quarto.quartoPreview, { silent = true, noremap = true, desc = "Open quarto preview" })
    vim.keymap.set({ "n" }, "<leader>qc", quarto.quartoClosePreview,
        { silent = true, noremap = true, desc = "Close quarto preview" })
    vim.keymap.set({ "n" }, "<leader>qw", quarto.quartoPreviewNoWatch,
        { silent = true, noremap = true, desc = "Open quarto preview (no watch)" })
    vim.keymap.set({ "n" }, "<leader>qu", quarto.quartoUpdatePreview,
        { silent = true, noremap = true, desc = "Update quarto preview" })


    local runner = require("quarto.runner")
    vim.keymap.set({ "n" }, "<c-cr>", runner.run_cell, { desc = "run cell", silent = true })
    vim.keymap.set({ "x" }, "<c-cr>", runner.run_range, { desc = "run visual range", silent = true })
    vim.keymap.set({ "n" }, "<cr>k", runner.run_above, { desc = "run cell and above", silent = true })
    vim.keymap.set({ "n" }, "<cr>a", runner.run_all, { desc = "run all cells", silent = true })
    vim.keymap.set({ "n" }, "<cr>A", function() runner.run_all(true) end, { desc = "run all cells of all languages", silent = true })
end

return {
    "quarto-dev/quarto-nvim",
    dependencies = {
        { "jmbuhr/otter.nvim" },
        {
            "benlubas/molten-nvim",
            dependencies = {
                { "3rd/image.nvim" },
            },
            build = ":UpdateRemotePlugins",
        },
    },
    config = quarto_setup
}
