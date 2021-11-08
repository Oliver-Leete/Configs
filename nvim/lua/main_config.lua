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
vim.opt.shell="/bin/zsh"

vim.cmd([[call plug#begin('~/.config/nvim/pluged')
    Plug 'nvim-lua/plenary.nvim'
    Plug 'tpope/vim-repeat'
    Plug 'kyazdani42/nvim-web-devicons'
    Plug 'nvim-lua/popup.nvim'
    Plug '907th/vim-auto-save'

    Plug 'knubie/vim-kitty-navigator'
    Plug 'kazhala/close-buffers.nvim'
    Plug 'tpope/vim-projectionist'

    Plug 'IndianBoy42/hop.nvim'
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
    Plug 'akinsho/nvim-bufferline.lua'
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
    Plug 'RRethy/vim-illuminate'
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
    Plug 'abecodes/tabout.nvim'

    Plug 'nvim-telescope/telescope.nvim'
    Plug 'nvim-telescope/telescope-hop.nvim'
    Plug 'nvim-telescope/telescope-symbols.nvim'
    Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make'}
    Plug 'nvim-telescope/telescope-media-files.nvim'
    Plug 'nvim-telescope/telescope-bibtex.nvim'
    Plug 'nvim-telescope/telescope-github.nvim'
    Plug 'crispgm/telescope-heading.nvim'

    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    Plug 'nvim-treesitter/nvim-treesitter-textobjects'
    Plug 'nvim-treesitter/playground'
    Plug 'p00f/nvim-ts-rainbow'
    Plug 'ThePrimeagen/refactoring.nvim'
call plug#end()]])


--Settings
vim.opt.nrformats = vim.opt.nrformats - "octal"
vim.opt.clipboard = vim.opt.clipboard + "unnamedplus"
vim.opt.viminfo = "'100,f1"
vim.opt.mouse = "a"
vim.opt.encoding = "UTF-8"
vim.opt.scrolloff = 0
vim.opt.updatetime = 100
vim.opt.backspace = "indent,eol,start"
vim.opt.diffopt = "internal,filler,closeoff,iwhite,context:100000000"
vim.opt.pumheight = 20
vim.opt.spelllang = "en_gb"
vim.opt.termguicolors = true
vim.opt.hidden = true
vim.opt.lazyredraw = true
vim.opt.shortmess = "Iflmnrwxt"
vim.opt.showmode = false

-- Search
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = true
vim.opt.inccommand = "split"
vim.opt.gdefault = true
vim.opt.incsearch = true

-- Indenting
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true

-- Wrapping
vim.opt.wrap = false
vim.opt.linebreak = true
vim.opt.breakindent = true
vim.opt.breakindentopt = "shift:2"
vim.opt.foldmethod = "expr"
vim.opt.foldlevel = 20

-- Numbering
vim.opt.signcolumn = "yes:2"
vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.cursorline = true
vim.cmd([[call matchadd('TabLine', '\%101v', 203)]])
vim.cmd([[augroup numbertoggle
    autocmd!
    autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu && mode() != "i" | set rnu   | endif
    autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu                  | set nornu | endif
    autocmd BufEnter,FocusGained,InsertLeave,WinEnter * setlocal cursorline
    autocmd BufLeave,FocusLost,InsertEnter,WinLeave * setlocal nocursorline
augroup END]])

-- Splitting
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.cmd([[augroup windowPositioning
    autocmd!
    autocmd FileType help :wincmd H | vertical resize 90<cr>
    autocmd FileType juliadoc wincmd H
    autocmd FileType qf wincmd J
augroup END]])

-- Yank
vim.cmd([[augroup LuaHighlight
    autocmd!
    autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank()
augroup END]])

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

-- Close Buffers Setup
require("close_buffers").setup({
    preserve_window_layout = {},
    -- preserve_window_layout = { "this" },
    next_buffer_cmd = function(windows)
        require("bufferline").cycle(1)
        local bufnr = vim.api.nvim_get_current_buf()

        for _, window in ipairs(windows) do
            vim.api.nvim_win_set_buf(window, bufnr)
        end
    end,
})

require("nvim_comment").setup({ comment_empty = false, line_mapping = "g,cc", operator_mapping = "g,c" })
require("hop").setup({ keys = "tnseriaodhgjplfuwybkvmcxzq" })
require("colorizer").setup({ "*" }, {
    RGB = true,
    RRGGBB = true,
    names = false,
    RRGGBBAA = true,
    rgb_fn = true,
    hsl_fn = true,
    css_fn = false,
    mode = "background",
})

