local quarto_setup = function()
    -- Image
    -- require("image").setup({
    --     backend = "kitty",
    --     integrations = {},
    --     max_width = 100,
    --     max_height = 12,
    --     max_height_window_percentage = math.huge,
    --     max_width_window_percentage = math.huge,
    --     window_overlap_clear_enabled = true,
    --     window_overlap_clear_ft_ignore = { "cmp_menu", "cmp_docs", "" },
    -- })

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
end

return {
    "quarto-dev/quarto-nvim",
    dependencies = {
        { "jmbuhr/otter.nvim" },
        {
            "benlubas/molten-nvim",
            dependencies = {
                -- { "3rd/image.nvim" },
            },
            build = ":UpdateRemotePlugins",
        },
    },
    config = quarto_setup,
    keys = {
        { "<leader>qq", function() require("quarto").quartoPreview() end,        desc = "Open quarto preview" },
        { "<leader>qc", function() require("quarto").quartoClosePreview() end,   desc = "Close quarto preview" },
        { "<leader>qw", function() require("quarto").quartoPreviewNoWatch() end, desc = "Open quarto preview (no watch)" },
        { "<leader>qu", function() require("quarto").quartoUpdatePreview() end,  desc = "Update quarto preview" },
        { "<c-cr>",     function() require("quarto.runner").run_cell() end,      desc = "run cell" },
        { "<c-cr>",     function() require("quarto.runner").run_range() end,     desc = "run visual range" },
    }
}
