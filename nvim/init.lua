vim.api.nvim_set_var("$SHELL", "/bin/zsh")
vim.opt.shell = "/bin/zsh"

vim.cmd([[call plug#begin('~/.config/nvim/pluged')
    Plug 'nvim-lua/plenary.nvim'
    Plug 'tpope/vim-repeat'
    Plug 'kyazdani42/nvim-web-devicons'
    Plug '907th/vim-auto-save'
    Plug 'anuvyklack/hydra.nvim'
    Plug 'anuvyklack/keymap-layer.nvim'
    Plug 'lewis6991/impatient.nvim'

    Plug 'knubie/vim-kitty-navigator'
    Plug 'stevearc/stickybuf.nvim'
    Plug 'tpope/vim-projectionist'
    Plug 'ThePrimeagen/harpoon'

    Plug 'echasnovski/mini.nvim'
    Plug 'ap/vim-you-keep-using-that-word'
    Plug 'phaazon/hop.nvim'
    Plug 'junegunn/vim-slash'
    Plug 'arthurxavierx/vim-caser'
    Plug 'ralismark/opsort.vim'
    Plug 'Konfekt/vim-CtrlXA'
    Plug 'svermeulen/vim-subversive'
    Plug 'inkarkat/vim-unconditionalpaste'
    Plug 'andymass/vim-matchup'

    Plug 'lervag/vimtex'
    Plug 'fladson/vim-kitty'
    Plug 'wilriker/gcode.vim'
    Plug 'LhKipp/nvim-nu'
    Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}

    Plug 'lewis6991/gitsigns.nvim'
    Plug 'sindrets/diffview.nvim'

    Plug 'rebelot/kanagawa.nvim'
    Plug 'lukas-reineke/indent-blankline.nvim'
    Plug 'stevearc/dressing.nvim'
    Plug 'rcarriga/nvim-notify'

    Plug 'mbbill/undotree'
    Plug 'kyazdani42/nvim-tree.lua'
    Plug 'folke/todo-comments.nvim'

    Plug 'williamboman/mason.nvim'
    Plug 'williamboman/mason-lspconfig.nvim'
    Plug 'jayp0521/mason-nvim-dap.nvim'

    Plug 'jose-elias-alvarez/null-ls.nvim'
    Plug 'neovim/nvim-lspconfig'
    Plug 'barreiroleo/ltex_extra.nvim'
    Plug 'p00f/clangd_extensions.nvim'
    Plug 'b0o/schemastore.nvim'

    Plug 'Issafalcon/lsp-overloads.nvim'
    Plug 'SmiteshP/nvim-navic'

    Plug 'hrsh7th/nvim-cmp'
    Plug 'hrsh7th/cmp-buffer'
    Plug 'hrsh7th/cmp-path'
    Plug 'hrsh7th/cmp-nvim-lua'
    Plug 'hrsh7th/cmp-nvim-lsp'
    Plug 'hrsh7th/cmp-omni'
    Plug 'petertriho/cmp-git'
    Plug 'hrsh7th/cmp-cmdline'
    Plug 'dmitmel/cmp-cmdline-history'
    Plug 'uga-rosa/cmp-dictionary'
    Plug 'mtoohey31/cmp-fish'
    Plug 'saadparwaiz1/cmp_luasnip'
    Plug 'kdheepak/cmp-latex-symbols'
    Plug 'kdheepak/cmp-latex-symbols'
    Plug 'rcarriga/cmp-dap'

    Plug 'L3MON4D3/LuaSnip'
    Plug 'danymat/neogen'

    Plug 'nvim-telescope/telescope.nvim'
    Plug 'nvim-telescope/telescope-file-browser.nvim'
    Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make'}

    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    Plug 'nvim-treesitter/playground'
    Plug 'nvim-treesitter/nvim-treesitter-textobjects'
    Plug 'ThePrimeagen/refactoring.nvim'
    Plug 'mizlan/iswap.nvim'
    Plug 'AckslD/nvim-trevJ.lua'

    Plug 'stevearc/overseer.nvim'
    Plug 'nvim-neotest/neotest'
    Plug 'andythigpen/nvim-coverage'
    Plug 't-troebst/perfanno.nvim'
    Plug 'krady21/compiler-explorer.nvim'
    Plug 'akinsho/toggleterm.nvim'
    Plug 'tknightz/telescope-termfinder.nvim'

    Plug 'mfussenegger/nvim-dap'
    Plug 'rcarriga/nvim-dap-ui'
    Plug 'theHamsta/nvim-dap-virtual-text'
    Plug 'nvim-telescope/telescope-dap.nvim'

    " Rust
    Plug 'simrat39/rust-tools.nvim'
    Plug 'rouge8/neotest-rust'

    " Python
    Plug 'nvim-neotest/neotest-python'
    Plug 'mfussenegger/nvim-dap-python'

    " Haskell
    Plug 'mrcjkb/neotest-haskell'
call plug#end()]])

pcall(require('impatient'))
OTerm = ""

pcall(require("user.settings_config"))
pcall(require("user.misc_config"))
pcall(require("user.mappings_config"))
pcall(require("user.command_config"))
pcall(require("user.jump_config"))
pcall(require("user.telescope_config"))
pcall(require("user.compleation_config"))
pcall(require("user.panels_config"))
pcall(require("user.myfuncs_config"))
pcall(require("user.lsp_config"))
pcall(require("user.dap_config"))
pcall(require("user.treesitter_config"))

pcall(require("user.ui_config"))
pcall(require("user.statusline_config"))
pcall(require("user.tabline_config"))
pcall(require("user.winbar_config"))

pcall(require("user.terminal_config"))
pcall(require("user.projects_config"))
pcall(require("user.overseer_config"))
pcall(require("user.testing_config"))

pcall(require("user.git_config"))
pcall(require("user.mini_config"))

pcall(require("user.filmpicker_config"))
