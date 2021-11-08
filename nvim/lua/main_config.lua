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

-- Saving
vim.opt.confirm = true
vim.opt.swapfile = false
vim.opt.undodir = "~/.vim/undo//"
vim.opt.undofile = true
vim.api.nvim_set_var("auto_save", 1)
vim.api.nvim_set_var("auto_save_silent", 1)

-- Search
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = true
vim.opt.inccommand = "split"
vim.opt.gdefault = true
vim.opt.incsearch = true
vim.api.nvim_set_keymap("", "<plug>(slash-after)", "<cmd>let g:dirJumps='search'<cr>zz", { noremap = false })

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

-- Set Filetype
vim.cmd([[augroup myfiletypes
    autocmd!
    au BufNewFile,BufRead *.fish set filetype=fish
    au BufNewFile,BufRead *.jl set filetype=julia
augroup end]])

require('telescope_config')
require('n_bindings_config')
require('x_bindings_config')
require('o_bindings_config')
require('i_bindings_config')
require('compleation_config')
require('panels_config')
require('myfuncs_config')
require('lsp_config')
require('treesitter_config')
require('git_config')
require('snippets_config')
require('bubble')
require('ui_config')

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

-- Targets Setup

vim.cmd([[augroup mywellle
    autocmd!
    autocmd User targets#mappings#user call targets#mappings#extend({
        \ '.': { 'separator': [{'d':','}, {'d':'.'}, {'d':';'}, {'d':':'}, {'d':'+'}, {'d':'-'},
        \                      {'d':'='}, {'d':'~'}, {'d':'_'}, {'d':'*'}, {'d':'#'}, {'d':'/'},
        \                      {'d':'\'}, {'d':'|'}, {'d':'&'}, {'d':'$'}] },
        \ ',': {},
        \ ';': {},
        \ ':': {},
        \ '+': {},
        \ '-': {},
        \ '=': {},
        \ '~': {},
        \ '_': {},
        \ '*': {},
        \ '#': {},
        \ '/': {},
        \ '\': {},
        \ '|': {},
        \ '&': {},
        \ '$': {},
        \ 'a': {'argument': [{'o': '[{([]', 'c': '[])}]', 's': '[,;]'}]},
        \ 'x': {'line': [{'c': 1}]},
        \ 't': {'tag': [{}]},
        \ '<': {'pair': [{'o': '<', 'c': '>'}]},
        \ '>': {'pair': [{'o': '>', 'c': '<'}]},
        \ 'b': {'pair': [{'o':'(', 'c':')'}, {'o':'[', 'c':']'}, {'o':'{', 'c':'}'}]},
        \ 'q': {'quote': [{'d':"'"}, {'d':'"'}, {'d':'`'}]},
        \ })
augroup end]])

vim.api.nvim_set_var("targets_seekRanges", "cc cr cb cB lc ac Ac lr rr ll lb ar ab lB Ar aB Ab AB rb rB al Al")
vim.api.nvim_set_var("targets_jumpRanges", "rr rb rB bb bB BB ll al Al aa Aa AA")
vim.api.nvim_set_var("targets_gracious", true)

-- Leader Mapping
vim.opt.timeoutlen = 500
vim.api.nvim_set_keymap("", "<BackSPACE>", "<Nop>", { noremap = true })
vim.api.nvim_set_keymap("", "<SPACE>", "<Nop>", { noremap = true })
vim.api.nvim_set_keymap("", "<BackSPACE>", "<Nop>", { noremap = true })
vim.api.nvim_set_var("mapleader", " ")
vim.api.nvim_set_var("maplocalleader", "\\")

-- Un-Mappings
vim.api.nvim_set_keymap("", "v", "<nop>", { nowait = true })
vim.api.nvim_set_keymap("", "V", "<nop>", { nowait = true })
vim.api.nvim_set_keymap("", "<c-v>", "<nop>", { nowait = true })
vim.api.nvim_set_keymap("", "z", "<nop>", { nowait = true })
vim.api.nvim_set_keymap("", "dd", "<nop>", { nowait = true })
vim.api.nvim_set_keymap("", "cc", "<nop>", { nowait = true })
vim.api.nvim_set_keymap("", "yy", "<nop>", { nowait = true })
vim.api.nvim_set_keymap("", "z", "<nop>", { nowait = true })

vim.api.nvim_set_keymap("", "Y", "<nop>", {})
vim.api.nvim_set_keymap("", "C", "<nop>", {})
vim.api.nvim_set_keymap("", "D", "<nop>", {})
vim.api.nvim_set_keymap("", "S", "<nop>", {})


-- NOTE: _, =, |, ^, ¬ and # are free to map

-- Mappings
vim.api.nvim_set_keymap("n", ";", ":", { noremap = true })
vim.api.nvim_set_keymap("n", ":", ";", { noremap = true })
vim.api.nvim_set_keymap("x", ";", ":", { noremap = true })
vim.api.nvim_set_keymap("x", ":", ";", { noremap = true })
vim.api.nvim_set_keymap("o", ";", ":", { noremap = true })
vim.api.nvim_set_keymap("o", ":", ";", { noremap = true })

vim.api.nvim_set_keymap("n", "<m-f>", ";", { noremap = true })
vim.api.nvim_set_keymap("n", "<m-F>", ",", { noremap = true })
vim.api.nvim_set_keymap("n", "<m-t>", ";", { noremap = true })
vim.api.nvim_set_keymap("n", "<m-T>", ",", { noremap = true })
vim.api.nvim_set_keymap("x", "<m-f>", ";", { noremap = true })
vim.api.nvim_set_keymap("x", "<m-F>", ",", { noremap = true })
vim.api.nvim_set_keymap("x", "<m-t>", ";", { noremap = true })
vim.api.nvim_set_keymap("x", "<m-T>", ",", { noremap = true })

vim.api.nvim_set_keymap("n", "g<c-a>", "v<c-a>", { noremap = true })
vim.api.nvim_set_keymap("n", "g<c-x>", "v<c-x>", { noremap = true })
vim.api.nvim_set_keymap("n", "+", "<c-a>", { noremap = true })
vim.api.nvim_set_keymap("n", "-", "<c-x>", { noremap = true })
vim.api.nvim_set_keymap("n", "g+", "v<c-a>", { noremap = true })
vim.api.nvim_set_keymap("n", "g-", "v<c-x>", { noremap = true })
vim.api.nvim_set_keymap("x", "+", "<c-a>", { noremap = true })
vim.api.nvim_set_keymap("x", "-", "<c-x>", { noremap = true })
vim.api.nvim_set_keymap("x", "g+", "g<c-a>", { noremap = true })
vim.api.nvim_set_keymap("x", "g-", "g<c-x>", { noremap = true })

vim.api.nvim_set_keymap("x", "y", "m1y`1", { noremap = true })

vim.api.nvim_set_keymap("n", "x", "V", { noremap = true })
vim.api.nvim_set_keymap("n", "X", "V", { noremap = true })
vim.api.nvim_set_keymap("n", "C", "<c-v>j", { noremap = true })
vim.api.nvim_set_keymap("n", "<m-C>", "<c-v>k", { noremap = true })
vim.api.nvim_set_keymap("n", "<M-v>", "v", { noremap = true })

vim.api.nvim_set_keymap("x", "x", "j$", { noremap = true })
vim.api.nvim_set_keymap("x", "X", "<esc>`<kV`>", { noremap = true })
vim.api.nvim_set_keymap("x", "C", "j", { noremap = true })
vim.api.nvim_set_keymap("x", "<m-C>", "<esc>`<k<c-v>`>", { noremap = true })
vim.api.nvim_set_keymap("x", "<M-v>", "v", { noremap = true })
vim.api.nvim_set_keymap("x", "<M-;>", "o", { noremap = true })

vim.api.nvim_set_keymap("n", "<m-c>", [["_c]], { noremap = true })
vim.api.nvim_set_keymap("n", "<m-d>", [["_d]], { noremap = true })
vim.api.nvim_set_keymap("x", "<m-c>", [["_c]], { noremap = true })
vim.api.nvim_set_keymap("x", "<m-d>", [["_d]], { noremap = true })

vim.api.nvim_set_keymap("n", "<m-c><m-c>", [["_cc]], { noremap = true })
vim.api.nvim_set_keymap("n", "<m-d><m-d>", [["_dd]], { noremap = true })
vim.api.nvim_set_keymap("x", "<m-c><m-c>", [["_cc]], { noremap = true })
vim.api.nvim_set_keymap("x", "<m-d><m-d>", [["_dd]], { noremap = true })

vim.api.nvim_set_keymap("n", "<m-o>", "m1o<esc>`1", { noremap = true })
vim.api.nvim_set_keymap("n", "<m-O>", "m1O<esc>`1", { noremap = true })

vim.api.nvim_set_keymap("n", ",", "<cmd>WhichKey g, n<cr>", { noremap = true })
vim.api.nvim_set_keymap("x", ",", "<cmd>WhichKey g, x<cr>", { noremap = true })
vim.api.nvim_set_keymap("x", "I", "I", { noremap = true })
vim.api.nvim_set_keymap("x", "A", "A", { noremap = true })

vim.api.nvim_set_keymap("n", "mm", "<cmd>lua require'harpoon.mark'.add_file()<cr>", { noremap = true })
vim.api.nvim_set_keymap("n", "ma", "<cmd>lua require'harpoon.ui'.nav_file(1)<cr>", { noremap = true })
vim.api.nvim_set_keymap("n", "mr", "<cmd>lua require'harpoon.ui'.nav_file(2)<cr>", { noremap = true })
vim.api.nvim_set_keymap("n", "ms", "<cmd>lua require'harpoon.ui'.nav_file(3)<cr>", { noremap = true })
vim.api.nvim_set_keymap("n", "mt", "<cmd>lua require'harpoon.ui'.nav_file(4)<cr>", { noremap = true })
vim.api.nvim_set_keymap("n", "M", "<cmd>lua require'harpoon.ui'.toggle_quick_menu()<cr>", { noremap = true })

vim.api.nvim_set_keymap("n", "<c-_>", "g,cc", {})
vim.api.nvim_set_keymap("x", "<c-_>", "g,c", {})

vim.api.nvim_set_keymap("n", "£", [[:exe "let @/='" . expand("<cWORD>") . "'"<cr>]], { noremap = true, silent = true })

vim.api.nvim_set_keymap("i", "<c-y>", [[matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]], { noremap = true, expr = true, nowait = true })
vim.api.nvim_set_keymap("i", "<c-l>", [[matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]], { noremap = true, expr = true, nowait = true })
vim.api.nvim_set_keymap("s", "<c-y>", [[matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]], { noremap = true, expr = true, nowait = true })
vim.api.nvim_set_keymap("s", "<c-l>", [[matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]], { noremap = true, expr = true, nowait = true })

vim.api.nvim_set_keymap("i", "<c-g>", "<c-o>%", { noremap = true })
vim.api.nvim_set_keymap("i", "<c-s>", "<cmd>lua require('lsp_signature').toggle_float_win()<CR>", { noremap = true })

vim.api.nvim_set_keymap("i", ",", ",<c-g>u", { noremap = true })
vim.api.nvim_set_keymap("i", ".", ".<c-g>u", { noremap = true })
vim.api.nvim_set_keymap("i", "!", "!<c-g>u", { noremap = true })
vim.api.nvim_set_keymap("i", "?", "?<c-g>u", { noremap = true })


-- Panel Specific Mappings
vim.cmd([[augroup panelMappings
    autocmd!
    autocmd filetype qf                  map <buffer> <esc> <cmd>q<cr>
    autocmd filetype help                map <buffer> <esc> <cmd>q<cr>
    autocmd filetype vim-plug            map <buffer> <esc> <cmd>q<cr>
    autocmd filetype juliadoc            map <buffer> <esc> <cmd>q<cr>
    autocmd filetype undotree            map <buffer> <esc> <cmd>UndotreeHide<cr>
    autocmd filetype lspinfo             map <buffer> <esc> <cmd>q<cr>
    autocmd filetype DiffviewFiles       map <buffer> <esc> <cmd>DiffviewClose<cr>
    autocmd filetype DiffviewFileHistory map <buffer> <esc> <cmd>DiffviewClose<cr>
    autocmd filetype tsplayground        map <buffer> <esc> <cmd>q<cr>
    autocmd filetype harpoon-menu        map <buffer> <esc> <cmd>wq<cr>
augroup END]])

vim.api.nvim_set_var("wordmotion_prefix", "$")

-- UnMap Plugins
vim.api.nvim_set_var("kitty_navigator_no_mappings", true)
vim.api.nvim_set_var("UnconditionalPaste_no_mappings", true)
vim.api.nvim_set_var("caser_no_mappings	=", true)
vim.api.nvim_set_var("textobj_markdown_no_default_key_mappings", true)
vim.api.nvim_set_var("julia_blocks", false)

vim.cmd([[fu! s:lastplace()
	if index(split("quickfix,nofile,help", ","), &buftype) != -1  | return | endif
	if index(split("gitcommit,gitrebase,svn,hgcommit", ","), &filetype) != -1 | return | endif
	try |
		if empty(glob(@%)) | return | endif
	catch | return | endtry
	if line("'\"") > 0 && line("'\"") <= line("$") | execute "normal! g`\"zz" | endif
endf
augroup lastplace_notplugin
	autocmd!
	autocmd BufWinEnter * call s:lastplace()
augroup END]])

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
