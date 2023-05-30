vim.api.nvim_set_var("$SHELL", "/bin/zsh")
vim.opt.shell = "/bin/zsh"
vim.g.neo_tree_remove_legacy_commands = true
vim.g.sexp_filetypes = "clojure,scheme,lisp,timl,fennel,janet"

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable",
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

pcall(require("user.settings"))

require("lazy").setup(
    {
        "nvim-lua/plenary.nvim",
        "tpope/vim-repeat",
        "kyazdani42/nvim-web-devicons",
        "907th/vim-auto-save",
        "anuvyklack/hydra.nvim",
        "anuvyklack/keymap-layer.nvim",
        "kevinhwang91/promise-async",

        "chrisgrieser/nvim-genghis",
        "stevearc/resession.nvim",
        "cbochs/grapple.nvim",
        "https://git.sr.ht/~marcc/BufferBrowser",

        "echasnovski/mini.nvim",
        "ap/vim-you-keep-using-that-word",
        "phaazon/hop.nvim",
        "junegunn/vim-slash",
        "arthurxavierx/vim-caser",
        "ralismark/opsort.vim",
        "Konfekt/vim-CtrlXA",
        "gbprod/substitute.nvim",
        "andymass/vim-matchup",
        "numToStr/Comment.nvim",
        "kana/vim-niceblock",

        "cshuaimin/ssr.nvim",
        "AckslD/muren.nvim",

        "lervag/vimtex",
        "iurimateus/luasnip-latex-snippets.nvim",
        "fladson/vim-kitty",
        "wilriker/gcode.vim",
        "LhKipp/nvim-nu",
        { "toppair/peek.nvim",     build = "deno task --quiet build:fast" },

        "lewis6991/gitsigns.nvim",
        "sindrets/diffview.nvim",
        "cvigilv/diferente.nvim",

        { "rebelot/kanagawa.nvim", commit = "4c8d48726621a7f3998c7ed35b2c2535abc22def" },
        "stevearc/dressing.nvim",
        "rcarriga/nvim-notify",
        "nvim-lualine/lualine.nvim",
        "MunifTanjim/nui.nvim",
        "folke/noice.nvim",
        "nvim-zh/colorful-winsep.nvim",

        "folke/todo-comments.nvim",
        "folke/trouble.nvim",

        "williamboman/mason.nvim",
        "williamboman/mason-lspconfig.nvim",
        "jayp0521/mason-nvim-dap.nvim",

        "jose-elias-alvarez/null-ls.nvim",
        "neovim/nvim-lspconfig",
        "barreiroleo/ltex_extra.nvim",
        "p00f/clangd_extensions.nvim",
        "b0o/schemastore.nvim",
        "LostNeophyte/null-ls-embedded",

        "SmiteshP/nvim-navic",
        "DNLHC/glance.nvim",
        "yioneko/nvim-type-fmt",
        "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
        "onsails/lspkind.nvim",
        "lvimuser/lsp-inlayhints.nvim",

        "hrsh7th/nvim-cmp",
        "hrsh7th/cmp-buffer",
        "hrsh7th/cmp-path",
        "hrsh7th/cmp-nvim-lua",
        "hrsh7th/cmp-nvim-lsp",
        "hrsh7th/cmp-omni",
        "petertriho/cmp-git",
        "hrsh7th/cmp-cmdline",
        "dmitmel/cmp-cmdline-history",
        "uga-rosa/cmp-dictionary",
        "mtoohey31/cmp-fish",
        "saadparwaiz1/cmp_luasnip",
        "L3MON4D3/cmp-luasnip-choice",
        "kdheepak/cmp-latex-symbols",
        "kdheepak/cmp-latex-symbols",
        "rcarriga/cmp-dap",

        "L3MON4D3/LuaSnip",
        "danymat/neogen",
        "windwp/nvim-autopairs",
        "abecodes/tabout.nvim",

        "nvim-telescope/telescope.nvim",
        "nvim-telescope/telescope-file-browser.nvim",
        {
            "nvim-telescope/telescope-fzf-native.nvim",
            build =
            "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build"
        },
        "debugloop/telescope-undo.nvim",
        "FeiyouG/command_center.nvim",
        "kkharji/sqlite.lua",
        "prochri/telescope-all-recent.nvim",
        { "nvim-treesitter/nvim-treesitter", build = ":TSInstall all | TSUpdate | TSUninstall comment" },
        "nvim-treesitter/playground",
        "nvim-treesitter/nvim-treesitter-textobjects",
        "ThePrimeagen/refactoring.nvim",
        "AckslD/nvim-trevJ.lua",
        "CKolkey/ts-node-action",
        "drybalka/tree-climber.nvim",

        "stevearc/overseer.nvim",
        "nvim-neotest/neotest",
        "andythigpen/nvim-coverage",
        "t-troebst/perfanno.nvim",
        "krady21/compiler-explorer.nvim",

        "mfussenegger/nvim-dap",
        "rcarriga/nvim-dap-ui",
        { "theHamsta/nvim-dap-virtual-text", branch = "inline-text" },
        "nvim-telescope/telescope-dap.nvim",
        "LiadOz/nvim-dap-repl-highlights",

        "simrat39/rust-tools.nvim",
        "rouge8/neotest-rust",

        "nvim-neotest/neotest-python",
        "mfussenegger/nvim-dap-python",

        "MrcJkb/haskell-tools.nvim",
        "mrcjkb/neotest-haskell",

        "guns/vim-sexp",
        "Olical/aniseed",
        -- "Olical/conjure",

        "folke/neodev.nvim",
        "nvim-neotest/neotest-plenary",
        {
            'quarto-dev/quarto-nvim',
            dependencies = {
                'jmbuhr/otter.nvim',
            },
        },
    },
    {
        install = {
            missing = true,
        },
        ui = {
            border = Border,
        },
        dev = {
            path = "~/Projects",
        },
    }
)

SendID = nil

pcall(require("user.misc"))
pcall(require("user.mappings"))
pcall(require("user.command"))
pcall(require("user.targets"))
pcall(require("user.telescope"))
pcall(require("user.compleation"))
pcall(require("user.myfuncs"))
pcall(require("user.lsp"))
pcall(require("user.treesitter"))
pcall(require("user.dap"))
pcall(require("user.panels"))

pcall(require("user.ui"))
pcall(require("user.statusline"))
pcall(require("user.tabline"))
pcall(require("user.winbar"))
pcall(require("user.statuscol"))

pcall(require("user.projects"))
pcall(require("user.overseer"))
pcall(require("user.testing"))

pcall(require("user.git"))
pcall(require("user.mini"))
pcall(require("user.noice"))

pcall(require("user.filmpicker"))

-- need to put this after targets
Map("x", "i", "<Plug>(niceblock-I)", { remap = true, nowait = true })
Map("x", "a", "<Plug>(niceblock-A)", { remap = true, nowait = true })
