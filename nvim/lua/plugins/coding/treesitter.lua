local ts_setup = function()
    require("nvim-treesitter.configs").setup({
        indent = { enable = false },
        highlight = {
            enable = true,
            addditional_vim_regex_highlighting = false,
            disable = function(lang)
                return lang == "tex" or lang == "latex"
            end,
        },
        query_linter = {
            enable = true,
            use_virtual_text = true,
            lint_events = { "BufWrite", "CursorHold" },
        },
    })

end

return {
    "nvim-treesitter/nvim-treesitter",
    build = { ":TSInstall all", ":TSUpdate all", ":TSUninstall comment" },
    config = ts_setup,
}
