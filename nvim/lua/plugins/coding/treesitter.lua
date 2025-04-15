---@module "lazy"
---@type LazySpec
return {
    {
        "nvim-treesitter/nvim-treesitter",
        build = { ":TSInstall all", ":TSUpdate all", ":TSUninstall comment" },
        opts = {
            indent = { enable = false },
            highlight = {
                enable = true,
                addditional_vim_regex_highlighting = false,
                disable = function(lang) return lang == "tex" or lang == "latex" end,
            },
            query_linter = {
                enable = true,
                use_virtual_text = true,
                lint_events = { "BufWrite", "CursorHold" },
            },
        },
        opts_extend = { "ensure_installed" },
        main = "nvim-treesitter.configs",
    },
}
