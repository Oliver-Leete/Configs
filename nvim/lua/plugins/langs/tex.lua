---@module "lazy"
---@type LazySpec
return {
    {
        "lervag/vimtex",
        lazy = false, -- lazy-loading will disable inverse search
        ft = { "tex", "plaintex", "bib" },
        dependencies = { "tpope/vim-repeat" },
        config = function()
            vim.g.vimtex_mappings_enabled = 0
            vim.g.vimtex_text_obj_enabled = 0
            vim.g.vimtex_imaps_enabled = 0
            vim.g.tex_flavor = "latex"
            vim.g.vimtex_quickfix_mode = 0
            vim.g.vimtex_doc_confirm_single = 0
            vim.g.vimtex_view_general_viewer = "zathura --synctex-editor-command='nvr --servername "
                .. vim.v.servername
                .. " +%{line} %{input}'"
            vim.g.vimtex_view_forward_search_on_start = 1
            vim.g.vimtex_view_automatic = 0
            vim.g.vimtex_compiler_latexmk = {
                ["callback"] = 1,
                ["continuous"] = 0,
                ["executable"] = "latexmk",
                ["hooks"] = {},
                ["options"] = { "-pdf", "-verbose", "-file-line-error", "-synctex=1", "-interaction=nonstopmode" },
            }
        end,
        keys = {
            {
                "<localleader><localleader>",
                function()
                    vim.cmd("TexlabForward")
                    vim.cmd("sleep 20m")
                    vim.cmd("silent !xdotool key Escape")
                    vim.cmd("sleep 200m")
                    vim.cmd("silent !xdotool key super+n")
                end,
                ft = "tex",
                desc = "Open Preview",
            },

            {
                "am",
                "<plug>(vimtex-a$)",
                ft = "tex",
                remap = true,
                mode = { "x", "o" },
            },
            {
                "im",
                "<plug>(vimtex-i$)",
                ft = "tex",
                remap = true,
                mode = { "x", "o" },
            },
            {
                "[m",
                "<cmd>let g:dirJumps='n'<cr>m`<plug>(vimtex-[n)zz",
                ft = "tex",
                remap = false,
                mode = { "n", "x", "o" },
            },
            {
                "alm",
                ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-[l)', '(vimtex-a$)')<cr>",
                ft = "tex",
                remap = false,
                mode = { "x", "o" },
            },
            {
                "ilm",
                ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-[l)', '(vimtex-i$)')<cr>",
                ft = "tex",
                remap = false,
                mode = { "x", "o" },
            },
            {
                "]m",
                "<cmd>let g:dirJumps='n'<cr>m`<plug>(vimtex-]n)zz",
                ft = "tex",
                remap = false,
                mode = { "n", "x", "o" },
            },
            {
                "anm",
                ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]n)', '(vimtex-a$)')<cr>",
                ft = "tex",
                remap = false,
                mode = { "x", "o" },
            },
            {
                "inm",
                ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]n)', '(vimtex-i$)')<cr>",
                ft = "tex",
                remap = false,
                mode = { "x", "o" },
            },

            {
                "<c-leftmouse>",
                "<cmd>TexlabForward<cr>",
                ft = "tex",
                remap = false,
            },

            {
                "dpe",
                "<plug>(vimtex-env-delete)",
                ft = "tex",
                remap = true,
            },
            {
                "dpc",
                "<plug>(vimtex-cmd-delete)",
                ft = "tex",
                remap = true,
            },
            {
                "dpd",
                "<plug>(vimtex-delim-delete)",
                ft = "tex",
                remap = true,
            },
            {
                "cpe",
                "<plug>(vimtex-env-change)",
                ft = "tex",
                remap = true,
            },
            {
                "cpc",
                "<plug>(vimtex-cmd-change)",
                ft = "tex",
                remap = true,
            },
            {
                "cpd",
                "<plug>(vimtex-delim-change)",
                ft = "tex",
                remap = true,
            },

            {
                "ype",
                "<plug>(vimtex-env-surround-operator)",
                ft = "tex",
                remap = true,
            },
            {
                "ype",
                "<plug>(vimtex-env-surround-visual)",
                ft = "tex",
                remap = true,
                mode = { "x" },
            },

            {
                "<localleader>d",
                "<plug>(vimtex-delim-add-modifiers)",
                ft = "tex",
                remap = true,
            },
            {
                "%",
                "<plug>(vimtex-%)",
                ft = "tex",
                remap = true,
            },

            {
                ",nm",
                "<plug>(vimtex-env-toggle-math)",
                ft = "tex",
                remap = true,
            },
            {
                ",nf",
                "<plug>(vimtex-cmd-toggle-frac)",
                ft = "tex",
                remap = true,
            },
            {
                ",nd",
                "<plug>(vimtex-delim-toggle-modifier)",
                ft = "tex",
                remap = true,
            },

            {
                "<localleader>K",
                "<plug>(vimtex-doc-package)",
                ft = "tex",
                desc = "Vimtex Docs",
            },
        },
    },
    {
        "neovim/nvim-lspconfig",
        ---@module "plugins.lsp"
        ---@type PluginLspOpts
        opts = {
            servers = {
                texlab = {
                    mason = true,
                    settings = {
                        texlab = {
                            build = {
                                args = { "-pdf", "-interaction=nonstopmode", "-synctex=1", "%f" },
                                executable = "latexmk",
                                onSave = false,
                                forwardSearchAfter = true,
                            },
                            forwardSearch = {
                                executable = "zathura",
                                args = { "--synctex-forward", "%l:1:%f", "%p" },
                                onSave = false,
                            },
                            chktex = {
                                onEdit = true,
                                onOpenAndSave = true,
                            },
                            latexFormatter = "latexindent",
                            latexindent = {
                                ["local"] = ".latexindent.yaml",
                                modifyLineBreaks = true,
                            },
                        },
                    },
                },
            },
        },
    },
}
