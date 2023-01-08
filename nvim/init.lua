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
    Plug 'kevinhwang91/promise-async'

    Plug 'knubie/vim-kitty-navigator'
    Plug 'stevearc/stickybuf.nvim'
    Plug 'famiu/bufdelete.nvim'
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
    Plug 'chrisgrieser/nvim-genghis'

    Plug 'cshuaimin/ssr.nvim/'

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
    Plug 'nvim-lualine/lualine.nvim'
    Plug 'MunifTanjim/nui.nvim'
    Plug 'folke/noice.nvim'
    Plug 'nvim-zh/colorful-winsep.nvim'

    Plug 'mbbill/undotree'
    Plug 'folke/todo-comments.nvim'
    Plug 'nvim-neo-tree/neo-tree.nvim'

    Plug 'williamboman/mason.nvim'
    Plug 'williamboman/mason-lspconfig.nvim'
    Plug 'jayp0521/mason-nvim-dap.nvim'

    Plug 'jose-elias-alvarez/null-ls.nvim'
    Plug 'neovim/nvim-lspconfig'
    Plug 'barreiroleo/ltex_extra.nvim'
    Plug 'p00f/clangd_extensions.nvim'
    Plug 'b0o/schemastore.nvim'
    Plug 'LostNeophyte/null-ls-embedded'

    Plug 'SmiteshP/nvim-navic'
    Plug 'camilledejoye/nvim-lsp-selection-range'
    Plug 'DNLHC/glance.nvim'
    Plug 'yioneko/nvim-type-fmt'

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
    Plug 'doxnit/cmp-luasnip-choice'
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
    Plug 'Wansmer/treesj'
    Plug 'CKolkey/ts-node-action'
    " Plug 'Dkendal/nvim-treeclimber'

    Plug 'stevearc/overseer.nvim'
    Plug 'nvim-neotest/neotest'
    Plug 'andythigpen/nvim-coverage'
    Plug 't-troebst/perfanno.nvim'
    Plug 'krady21/compiler-explorer.nvim'
    Plug 'akinsho/toggleterm.nvim'

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
    Plug 'MrcJkb/haskell-tools.nvim'
    Plug 'mrcjkb/neotest-haskell'

    " Lua
    Plug 'folke/neodev.nvim'
    Plug 'nvim-neotest/neotest-plenary'
call plug#end()]])

pcall(require('impatient'))
OTerm = nil
STerm = nil

pcall(require("user.settings"))
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

pcall(require("user.terminal"))
pcall(require("user.projects"))
pcall(require("user.overseer"))
pcall(require("user.testing"))

pcall(require("user.git"))
pcall(require("user.mini"))
pcall(require("user.noice"))

pcall(require("user.filmpicker"))
