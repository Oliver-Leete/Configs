""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                       _   _   ______    ____   __      __  _____   __  __                        "
"                      | \ | | |  ____|  / __ \  \ \    / / |_   _| |  \/  |                       "
"                      |  \| | | |__    | |  | |  \ \  / /    | |   | \  / |                       "
"                      | . ` | |  __|   | |  | |   \ \/ /     | |   | |\/| |                       "
"                      | |\  | | |____  | |__| |    \  /     _| |_  | |  | |                       "
"                      |_| \_| |______|  \____/      \/     |_____| |_|  |_|                       "
"                                                                                                  "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Oliver Leete <oliverleete@gmail.com>                                                             "
" https://github.com/oliver-leete                                                                  "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let $SHELL = "/bin/zsh"
set shell=/bin/zsh

call plug#begin('~/.config/nvim/pluged')
    Plug 'nvim-lua/plenary.nvim'
    Plug 'tpope/vim-repeat'
    Plug 'kyazdani42/nvim-web-devicons'
    Plug 'nvim-lua/popup.nvim'
    Plug '907th/vim-auto-save'
    Plug 'knubie/vim-kitty-navigator'
    Plug 'kazhala/close-buffers.nvim'
    Plug 'tpope/vim-projectionist'

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

    " Hop, Skip And Jump
    Plug 'IndianBoy42/hop.nvim'
    Plug 'mizlan/iswap.nvim'

call plug#end()

" !!SETTINGS!!
noremap <plug>(slash-after) <cmd>let g:dirJumps='search'<cr>zz

" Saving and Backup
set confirm
set noswapfile
set undodir=~/.vim/undo//
set undofile
let g:auto_save = 1  " enable AutoSave on Vim startup
let g:auto_save_silent = 1  " do not display the auto-save notification


" Wellle Tagets settings
augroup mywellle
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
augroup end
" let g:targets_seekRanges = 'cc cr cb cB lc ac Ac lr rr ll lb ar ab lB Ar aB Ab AB rb rB al Al'
let g:targets_seekRanges = 'cc cr cb cB lc ac Ac lr rr ll lb ar ab lB Ar aB Ab AB rb rB al Al'
let g:targets_jumpRanges = 'rr rb rB bb bB BB ll al Al aa Aa AA'
let g:targets_gracious = 1
let targets_nl = 'nN'

" highlight Yank
augroup LuaHighlight
    autocmd!
    autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank()
augroup END

" Leader key remap
set timeoutlen=500
nnoremap <SPACE> <Nop>
nnoremap <BackSPACE> <Nop>
xnoremap <SPACE> <Nop>
xnoremap <BackSPACE> <Nop>
let mapleader = "\<space>"
let gaplocalleader = "\\"

" NOTE: _, =, |, ^, ¬ and # are free to map
" !!MAPPINGS!!
map <nowait> v <nop>
map V <nop>
map <c-v> <nop>

nmap <nowait> z <nop>
nmap <nowait> dd <nop>
nmap <nowait> cc <nop>
nmap <nowait> yy <nop>
nmap <nowait> z <nop>
nmap <nowait> v <nop>
nmap <nowait> V <nop>
nmap <nowait> <c-v> <nop>

map Y <nop>
map C <nop>
map D <nop>
map S <nop>

nnoremap ; :
nnoremap : ;
xnoremap ; :
xnoremap : ;
onoremap ; :
onoremap : ;

nnoremap g<c-a> v<c-a>
nnoremap g<c-x> v<c-x>
nnoremap + <c-a>
nnoremap - <c-x>
nnoremap g+ v<c-a>
nnoremap g- v<c-x>
xnoremap + <c-a>
xnoremap - <c-x>
xnoremap g+ g<c-a>
xnoremap g- g<c-x>

xnoremap y m1y`1

" slowly move to kak mappings
nnoremap x V
nnoremap X V
nnoremap C <c-v>j
nnoremap <m-C> <c-v>k
nnoremap <M-v> v

xnoremap x j$
xnoremap X <esc>`<kV`>
xnoremap C j
xnoremap <m-C> <esc>`<k<c-v>`>
xnoremap <M-v> v
xnoremap <M-;> o

nnoremap <m-c> "_c
nnoremap <m-d> "_d
xnoremap <m-c> "_c
xnoremap <m-d> "_d

nnoremap <m-c><m-c> "_cc
nnoremap <m-d><m-d> "_dd
xnoremap <m-c><m-c> "_cc
xnoremap <m-d><m-d> "_dd

nnoremap <m-o> m1o<esc>`1
nnoremap <m-O> m1O<esc>`1

nnoremap , <cmd>WhichKey g, n<cr>
xnoremap , <cmd>WhichKey g, x<cr>
xnoremap I I
xnoremap A A

nnoremap [ <cmd>WhichKey [ n<cr>
nnoremap ] <cmd>WhichKey ] n<cr>

nnoremap mm <cmd>lua require("harpoon.mark").add_file()<cr>
nnoremap ma <cmd>lua require("harpoon.ui").nav_file(1)<cr>
nnoremap mr <cmd>lua require("harpoon.ui").nav_file(2)<cr>
nnoremap ms <cmd>lua require("harpoon.ui").nav_file(3)<cr>
nnoremap mt <cmd>lua require("harpoon.ui").nav_file(4)<cr>
nnoremap M <cmd>lua require("harpoon.ui").toggle_quick_menu()<cr>


" Insert Mode
inoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')
inoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')
snoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')
snoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')

" Undo breakpoints
inoremap , ,<c-g>u
inoremap . .<c-g>u
inoremap ! !<c-g>u
inoremap ? ?<c-g>u

inoremap <silent><expr> <plug>(compe-close) compe#close('<c-e>')
snoremap <silent><expr> <plug>(compe-close) compe#close('<c-e>')

imap <c-g> <c-o>%
inoremap <c-s> <cmd>lua require('lsp_signature').toggle_float_win()<CR>

inoremap        <c-a> <C-O>^
cnoremap        <c-a> <Home>
inoremap        <c-e> <End>
cnoremap        <c-e> <End>

" Panel Specific Mappings
augroup panelMappings
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
augroup END

noremap <silent> £ :exe "let @/='" . expand("<cWORD>") . "'"<cr>

nnoremap <m-f> ;
nnoremap <m-F> ,
nnoremap <m-t> ;
nnoremap <m-T> ,
xnoremap <m-f> ;
xnoremap <m-F> ,
xnoremap <m-t> ;
xnoremap <m-T> ,

" Word Motion Command
let g:wordmotion_prefix = '$'

" Nvim Comment
nmap <c-_> g,cc
xmap <c-_> g,c

" Unmap Pluggins
let g:kitty_navigator_no_mappings = 1
let g:UnconditionalPaste_no_mappings = 1
let g:caser_no_mappings	= 1
let g:textobj_markdown_no_default_key_mappings=1
let g:julia_blocks=0

"List Clearing mappings
command! CClear cexpr[]
command! LClear lexpr[]

" Set Filetypes
augroup myfiletypes
    autocmd!
    au BufNewFile,BufRead *.fish set filetype=fish
    au BufNewFile,BufRead *.jl set filetype=julia
augroup end

" Save a single (but small) plugin
let g:lastplace_ignore = "gitcommit,gitrebase,svn,hgcommit"
let g:lastplace_open_folds = 1
let g:lastplace_ignore_buftype = "quickfix,nofile,help"

fu! s:lastplace()
	if index(split(g:lastplace_ignore_buftype, ","), &buftype) != -1  | return | endif
	if index(split(g:lastplace_ignore, ","), &filetype) != -1 | return | endif
	try |
		if empty(glob(@%)) | return | endif
	catch | return | endtry
	if line("'\"") > 0 && line("'\"") <= line("$") | execute "normal! g`\"zz" | endif
endf
augroup lastplace_notplugin
	autocmd!
	autocmd BufWinEnter * call s:lastplace()
augroup END

lua << EOF
require('telescope_config')
require('n_bindings_config')
require('x_bindings_config')
require('o_bindings_config')
require('i_bindings_config')
require('main_config')
require('compleation_config')
require('panels_config')
require('myfuncs_config')
require('lsp_config')
require('treesitter_config')
require('git_config')
require('snippets_config')
require('bubble')
require('ui_config')
EOF

redraw
