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

    require("ts-node-action").setup({
        julia = require("ts-node-action.filetypes.julia")
    })

    require("null-ls").register({
        name = "more_actions",
        method = { require("null-ls").methods.CODE_ACTION },
        filetypes = { "_all" },
        generator = {
            fn = require("ts-node-action").available_actions
        }
    })
end

return {
    "nvim-treesitter/nvim-treesitter",
    build = { ":TSInstall all", ":TSUpdate all", ":TSUninstall comment" },
    dependencies = {
        { "CKolkey/ts-node-action", dependencies = { "tpope/vim-repeat" } },
    },
    config = ts_setup,
}
