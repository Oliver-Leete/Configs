----------------------------------------------------------------------------------------------------
--                      _   _   ______    ____   __      __  _____   __  __                       --
--                     | \ | | |  ____|  / __ \  \ \    / / |_   _| |  \/  |                      --
--                     |  \| | | |__    | |  | |  \ \  / /    | |   | \  / |                      --
--                     | . ` | |  __|   | |  | |   \ \/ /     | |   | |\/| |                      --
--                     | |\  | | |____  | |__| |    \  /     _| |_  | |  | |                      --
--                     |_| \_| |______|  \____/      \/     |_____| |_|  |_|                      --
--                                                                                                --
----------------------------------------------------------------------------------------------------
-- Oliver Leete <oliverleete@gmail.com>                                                            --
-- https://github.com/oliver-leete                                                                 --
----------------------------------------------------------------------------------------------------

vim.api.nvim_set_var("$SHELL", "/bin/zsh")
vim.opt.shell = "/bin/zsh"

vim.cmd([[call plug#begin('~/.config/nvim/pluged')
    Plug 'nvim-lua/plenary.nvim'
    Plug 'tpope/vim-repeat'
    Plug 'kyazdani42/nvim-web-devicons'
    Plug 'nvim-lua/popup.nvim'
    Plug '907th/vim-auto-save'
    Plug 'b0o/mapx.nvim'

    Plug 'knubie/vim-kitty-navigator'
    Plug 'kazhala/close-buffers.nvim'
    Plug 'tpope/vim-projectionist'

    Plug 'phaazon/hop.nvim'
    Plug 'mizlan/iswap.nvim'
    Plug 'chaoren/vim-wordmotion'
    Plug 'junegunn/vim-slash'
    Plug 'ThePrimeagen/harpoon'
    Plug 'numToStr/Comment.nvim'
    Plug 'arthurxavierx/vim-caser'
    Plug 'junegunn/vim-easy-align'
    Plug 'Konfekt/vim-CtrlXA'
    Plug 'svermeulen/vim-subversive'
    Plug 'inkarkat/vim-unconditionalpaste'
    Plug 'tpope/vim-abolish'
    Plug 'tpope/vim-surround'

    Plug 'tommcdo/vim-ninja-feet'
    Plug 'wellle/targets.vim'
    Plug 'wellle/line-targets.vim'

    Plug 'lervag/vimtex'
    Plug 'anufrievroman/vim-angry-reviewer'
    Plug 'JuliaEditorSupport/julia-vim'
    Plug 'fladson/vim-kitty'
    Plug 'simrat39/rust-tools.nvim'
    Plug 'saecki/crates.nvim'
    Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
    Plug 'renerocksai/telekasten.nvim'

    Plug 'rebelot/kanagawa.nvim'
    Plug 'folke/tokyonight.nvim'
    Plug 'folke/zen-mode.nvim'
    Plug 'norcalli/nvim-colorizer.lua'
    Plug 'lewis6991/gitsigns.nvim'
    Plug 'windwp/windline.nvim'

    Plug 'mbbill/undotree'
    Plug 'folke/trouble.nvim'
    Plug 'folke/todo-comments.nvim'
    Plug 'folke/which-key.nvim'
    Plug 'sindrets/diffview.nvim'

    Plug 'neovim/nvim-lspconfig'
    Plug 'williamboman/nvim-lsp-installer'
    Plug 'jose-elias-alvarez/null-ls.nvim'
    Plug 'ray-x/lsp_signature.nvim'
    Plug 'j-hui/fidget.nvim'
    Plug 'brymer-meneses/grammar-guard.nvim'

    Plug 'hrsh7th/nvim-cmp'
    Plug 'hrsh7th/cmp-buffer'
    Plug 'hrsh7th/cmp-path'
    Plug 'hrsh7th/cmp-nvim-lua'
    Plug 'hrsh7th/cmp-nvim-lsp'
    Plug 'hrsh7th/cmp-omni'
    Plug 'petertriho/cmp-git'
    Plug 'hrsh7th/cmp-cmdline'
    Plug 'uga-rosa/cmp-dictionary'
    Plug 'mtoohey31/cmp-fish'

    Plug 'L3MON4D3/LuaSnip'
    Plug 'danymat/neogen'
    Plug 'saadparwaiz1/cmp_luasnip'
    Plug 'rafamadriz/friendly-snippets'
    Plug 'windwp/nvim-autopairs'
    Plug 'abecodes/tabout.nvim'

    Plug 'nvim-telescope/telescope-file-browser.nvim'
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'nvim-telescope/telescope-symbols.nvim'
    Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make'}
    Plug 'nvim-telescope/telescope-media-files.nvim'
    Plug 'nvim-telescope/telescope-bibtex.nvim'
    Plug 'nvim-telescope/telescope-github.nvim'
    Plug 'crispgm/telescope-heading.nvim'
    Plug 'rudism/telescope-dict.nvim'
    Plug 'gbrlsnchs/telescope-lsp-handlers.nvim'
    Plug 'stevearc/dressing.nvim'

    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    Plug 'nvim-treesitter/nvim-treesitter-textobjects'
    Plug 'nvim-treesitter/playground'
    Plug 'ThePrimeagen/refactoring.nvim'

    Plug 'mfussenegger/nvim-dap'
    Plug 'Pocco81/DAPInstall.nvim'
    Plug 'rcarriga/nvim-dap-ui'
    Plug 'theHamsta/nvim-dap-virtual-text'
    Plug 'nvim-telescope/telescope-dap.nvim'
    Plug 't-troebst/perfanno.nvim'
    Plug 'andythigpen/nvim-coverage'
call plug#end()]])

-- Disable builtins
local disabled_built_ins = {
    "netrw",
    "netrwPlugin",
    "netrwSettings",
    "netrwFileHandlers",
    "gzip",
    "zip",
    "zipPlugin",
    "tar",
    "tarPlugin",
    "getscript",
    "getscriptPlugin",
    "vimball",
    "vimballPlugin",
    "2html_plugin",
    "logipat",
    "rrhelper",
    "spellfile_plugin",
    "matchit",
}

for _, plugin in pairs(disabled_built_ins) do
    vim.g["loaded_" .. plugin] = 1
end


require('settings_config')
require('misc_config')
require('mappings_config')
-- require('visual-multi')
require('jump_config')
require('telescope_config')
require('compleation_config')
require('panels_config')
require('myfuncs_config')
require('lsp_config')
require('treesitter_config')
require('dap_config')
require('git_config')
require('snippets_config')
require('bubble')
require('ui_config')


local enterAndExitVim = vim.api.nvim_create_augroup("enterAndExitVim", { clear = true})
vim.api.nvim_create_autocmd("VimLeave", { command = 'silent! !kitty @ set-window-title ""', group = enterAndExitVim})
