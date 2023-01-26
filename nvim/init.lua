vim.api.nvim_set_var("$SHELL", "/bin/zsh")
vim.opt.shell = "/bin/zsh"

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
        { "knubie/vim-kitty-navigator" },
        "stevearc/stickybuf.nvim",
        "famiu/bufdelete.nvim",
        "tpope/vim-projectionist",
        "ThePrimeagen/harpoon",

        "echasnovski/mini.nvim",
        "ap/vim-you-keep-using-that-word",
        "phaazon/hop.nvim",
        "junegunn/vim-slash",
        "arthurxavierx/vim-caser",
        "ralismark/opsort.vim",
        "Konfekt/vim-CtrlXA",
        "svermeulen/vim-subversive",
        "andymass/vim-matchup",
        "numToStr/Comment.nvim",

        "cshuaimin/ssr.nvim",

        "lervag/vimtex",
        "iurimateus/luasnip-latex-snippets.nvim",
        "fladson/vim-kitty",
        "wilriker/gcode.vim",
        "LhKipp/nvim-nu",
        { "toppair/peek.nvim", build = "deno task --quiet build:fast" },

        "lewis6991/gitsigns.nvim",
        "sindrets/diffview.nvim",

        "rebelot/kanagawa.nvim",
        "stevearc/dressing.nvim",
        "rcarriga/nvim-notify",
        "nvim-lualine/lualine.nvim",
        "MunifTanjim/nui.nvim",
        "folke/noice.nvim",
        "nvim-zh/colorful-winsep.nvim",

        "folke/todo-comments.nvim",

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
        "camilledejoye/nvim-lsp-selection-range",
        "DNLHC/glance.nvim",
        "yioneko/nvim-type-fmt",

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
        "doxnit/cmp-luasnip-choice",
        "kdheepak/cmp-latex-symbols",
        "kdheepak/cmp-latex-symbols",
        "rcarriga/cmp-dap",

        "L3MON4D3/LuaSnip",
        "danymat/neogen",
        "windwp/nvim-autopairs",

        "nvim-telescope/telescope.nvim",
        "nvim-telescope/telescope-file-browser.nvim",
        { "nvim-telescope/telescope-fzf-native.nvim",
            build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build" },
        "debugloop/telescope-undo.nvim",
        "FeiyouG/command_center.nvim",

        { "nvim-treesitter/nvim-treesitter", build = ":TSInstall all | TSUpdate" },
        "nvim-treesitter/playground",
        "nvim-treesitter/nvim-treesitter-textobjects",
        "ThePrimeagen/refactoring.nvim",
        "mizlan/iswap.nvim",
        "AckslD/nvim-trevJ.lua",
        "Wansmer/treesj",
        "CKolkey/ts-node-action",

        "stevearc/overseer.nvim",
        "nvim-neotest/neotest",
        "andythigpen/nvim-coverage",
        "t-troebst/perfanno.nvim",
        "krady21/compiler-explorer.nvim",
        "RaafatTurki/hex.nvim",
        "akinsho/toggleterm.nvim",

        "mfussenegger/nvim-dap",
        "rcarriga/nvim-dap-ui",
        "theHamsta/nvim-dap-virtual-text",
        "nvim-telescope/telescope-dap.nvim",

        "simrat39/rust-tools.nvim",
        "rouge8/neotest-rust",

        "nvim-neotest/neotest-python",
        "mfussenegger/nvim-dap-python",

        "MrcJkb/haskell-tools.nvim",
        "mrcjkb/neotest-haskell",

        "folke/neodev.nvim",
        "nvim-neotest/neotest-plenary",
        { 'quarto-dev/quarto-nvim',
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
            icons = {
                cmd = " ",
                config = "",
                event = "",
                start = " ",
            },
        },
        dev = {
            path = "~/Projects",
        },
    }
)

OTerm = nil
STerm = nil

pcall(require("user.misc"))
pcall(require("user.mappings"))
pcall(require("user.command"))
pcall(require("user.targets"))
pcall(require("user.telescope"))
pcall(require("user.compleation"))
pcall(require("user.panels"))
pcall(require("user.myfuncs"))
pcall(require("user.lsp"))
pcall(require("user.dap"))
pcall(require("user.treesitter"))

pcall(require("user.ui"))
pcall(require("user.statusline"))
pcall(require("user.tabline"))
pcall(require("user.winbar"))
pcall(require("user.statuscol"))

pcall(require("user.terminal"))
pcall(require("user.projects"))
pcall(require("user.overseer"))
pcall(require("user.testing"))

pcall(require("user.git"))
pcall(require("user.mini"))
pcall(require("user.noice"))

pcall(require("user.filmpicker"))
