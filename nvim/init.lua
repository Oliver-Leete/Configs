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
    Plug 'tpope/vimrepeat'
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
    Plug 'terrortylor/nvim-comment'
    Plug 'arthurxavierx/vim-caser'
    Plug 'junegunn/vim-easy-align'
    Plug 'Konfekt/vim-CtrlXA'
    Plug 'svermeulen/vim-subversive'
    Plug 'inkarkat/vim-unconditionalpaste'
    Plug 'tpope/vim-abolish'

    Plug 'tommcdo/vim-ninja-feet'
    Plug 'wellle/targets.vim'
    Plug 'wellle/line-targets.vim'
    Plug 'kana/vim-textobj-user'
    Plug 'kana/vim-textobj-entire'
    Plug 'preservim/vim-textobj-sentence'

    Plug 'lervag/vimtex'
    Plug 'JuliaEditorSupport/julia-vim'
    Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
    Plug 'coachshea/vim-textobj-markdown'
    Plug 'fladson/vim-kitty'
    Plug 'anufrievroman/vim-angry-reviewer'

    Plug 'folke/tokyonight.nvim'
    Plug 'folke/zen-mode.nvim'
    Plug 'lukas-reineke/indent-blankline.nvim',
    Plug 'norcalli/nvim-colorizer.lua'
    Plug 'windwp/windline.nvim'
    Plug 'lewis6991/gitsigns.nvim'

    Plug 'mbbill/undotree'
    Plug 'folke/trouble.nvim'
    Plug 'folke/todo-comments.nvim'
    Plug 'folke/which-key.nvim'
    Plug 'sindrets/diffview.nvim'

    Plug 'neovim/nvim-lspconfig'
    Plug 'kabouzeid/nvim-lspinstall'
    Plug 'jose-elias-alvarez/null-ls.nvim'
    Plug 'ray-x/lsp_signature.nvim'

    Plug 'hrsh7th/nvim-cmp'
    Plug 'hrsh7th/cmp-buffer'
    Plug 'hrsh7th/cmp-path'
    Plug 'hrsh7th/cmp-nvim-lua'
    Plug 'hrsh7th/cmp-nvim-lsp'
    Plug 'tzachar/cmp-tabnine', { 'do': './install.sh' }
    Plug 'petertriho/cmp-git'
    Plug 'hrsh7th/cmp-cmdline'
    Plug 'L3MON4D3/LuaSnip'
    Plug 'saadparwaiz1/cmp_luasnip'
    Plug 'rafamadriz/friendly-snippets'
    Plug 'windwp/nvim-autopairs'

    Plug 'nvim-telescope/telescope.nvim'
    Plug 'nvim-telescope/telescope-symbols.nvim'
    Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make'}
    Plug 'nvim-telescope/telescope-media-files.nvim'
    Plug 'nvim-telescope/telescope-bibtex.nvim'
    Plug 'nvim-telescope/telescope-github.nvim'
    Plug 'crispgm/telescope-heading.nvim'

    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    Plug 'nvim-treesitter/nvim-treesitter-textobjects'
    Plug 'nvim-treesitter/playground'
    Plug 'ThePrimeagen/refactoring.nvim'
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
require('telescope_config')
require('compleation_config')
require('panels_config')
require('myfuncs_config')
require('lsp_config')
require('treesitter_config')
require('git_config')
require('snippets_config')
require('bubble')
require('ui_config')

vim.cmd([[augroup exitingVim
    autocmd VimLeave * silent! !kitty @ set-window-title ""
augroup end]])
