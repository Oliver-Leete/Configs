vim.api.nvim_set_var("$SHELL", "/bin/zsh")
vim.opt.shell = "/bin/zsh"

vim.cmd([[call plug#begin('~/.config/nvim/pluged')
    Plug 'nvim-lua/plenary.nvim'
    Plug 'tpope/vim-repeat'
    Plug 'kyazdani42/nvim-web-devicons'
    Plug '907th/vim-auto-save'

    Plug 'stevearc/dressing.nvim'
    Plug 'rcarriga/nvim-notify'

    Plug 'echasnovski/mini.nvim'

    Plug 'knubie/vim-kitty-navigator'
    Plug 'kazhala/close-buffers.nvim'
    Plug 'stevearc/stickybuf.nvim'
    Plug 'tpope/vim-projectionist'
    Plug 'ThePrimeagen/harpoon'

    Plug 'ap/vim-you-keep-using-that-word'
    Plug 'phaazon/hop.nvim'
    Plug 'chaoren/vim-wordmotion'
    Plug 'junegunn/vim-slash'
    Plug 'arthurxavierx/vim-caser'
    Plug 'junegunn/vim-easy-align'
    Plug 'ralismark/opsort.vim'
    Plug 'Konfekt/vim-CtrlXA'
    Plug 'svermeulen/vim-subversive'
    Plug 'inkarkat/vim-unconditionalpaste'
    Plug 'tpope/vim-abolish'
    Plug 'andymass/vim-matchup'
    Plug 'tommcdo/vim-nowchangethat'

    Plug 'lervag/vimtex'
    Plug 'JuliaEditorSupport/julia-vim'
    Plug 'fladson/vim-kitty'
    Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
    Plug 'wilriker/gcode.vim'
    Plug 'LhKipp/nvim-nu'

    Plug 'Oliver-Leete/gitsigns.nvim'
    Plug 'sindrets/diffview.nvim'
    Plug 'akinsho/git-conflict.nvim'

    Plug 'rebelot/kanagawa.nvim'
    Plug 'lukas-reineke/indent-blankline.nvim'

    Plug 'mbbill/undotree'
    Plug 'kyazdani42/nvim-tree.lua'
    Plug 'folke/todo-comments.nvim'

    Plug 'williamboman/mason.nvim'
    Plug 'williamboman/mason-lspconfig.nvim'
    Plug 'jose-elias-alvarez/null-ls.nvim'

    Plug 'neovim/nvim-lspconfig'
    Plug 'barreiroleo/ltex_extra.nvim'
    Plug 'simrat39/rust-tools.nvim'
    Plug 'p00f/clangd_extensions.nvim'
    Plug 'b0o/schemastore.nvim'

    Plug 'Issafalcon/lsp-overloads.nvim'
    Plug 'SmiteshP/nvim-navic'
    Plug 'https://git.sr.ht/~whynothugo/lsp_lines.nvim'

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

    Plug 'L3MON4D3/LuaSnip'
    Plug 'danymat/neogen'

    Plug 'nvim-telescope/telescope-file-browser.nvim'
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make'}
    Plug 'crispgm/telescope-heading.nvim'
    Plug 'gbrlsnchs/telescope-lsp-handlers.nvim'

    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    Plug 'nvim-treesitter/playground'
    Plug 'nvim-treesitter/nvim-treesitter-textobjects'
    Plug 'ThePrimeagen/refactoring.nvim'
    Plug 'mizlan/iswap.nvim'
    Plug 'AckslD/nvim-trevJ.lua'

    Plug 'stevearc/overseer.nvim'
    Plug 't-troebst/perfanno.nvim'
    Plug 'nvim-neotest/neotest'
    Plug 'rouge8/neotest-rust'
    Plug 'nvim-neotest/neotest-python'
    Plug 'andythigpen/nvim-coverage'

    Plug 'mfussenegger/nvim-dap'
    Plug 'Pocco81/DAPInstall.nvim'
    Plug 'rcarriga/nvim-dap-ui'
    Plug 'theHamsta/nvim-dap-virtual-text'
    Plug 'nvim-telescope/telescope-dap.nvim'

    Plug 'akinsho/toggleterm.nvim'
    Plug 'tknightz/telescope-termfinder.nvim'

    Plug 'anuvyklack/hydra.nvim'
    Plug 'anuvyklack/keymap-layer.nvim'
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

require("settings_config")
require("misc_config")
require("mappings_config")
require("jump_config")
require("telescope_config")
require("compleation_config")
require("panels_config")
require("myfuncs_config")
require("lsp_config")
require("treesitter_config")
require("ui_config")
require("statusline_config")
require("tabline_config")
require("winbar_config")
require("git_config")
require("terminal_config")
require("projects_config")
require("mini_config")

local enterAndExitVim = vim.api.nvim_create_augroup("enterAndExitVim", { clear = true })
vim.api.nvim_create_autocmd("VimEnter", { command = 'silent! !/home/oleete/.config/bin/nvrRename', group = enterAndExitVim })
vim.api.nvim_create_autocmd("VimLeave", { command = 'silent! !kitty @ set-window-title ""', group = enterAndExitVim })
