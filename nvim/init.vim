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
    " Misc and Dependencies
    Plug 'nvim-lua/plenary.nvim'
    Plug 'tpope/vim-repeat'
    Plug 'kyazdani42/nvim-web-devicons'
    Plug 'nvim-lua/popup.nvim'

    "Kitty
    Plug 'knubie/vim-kitty-navigator'
    Plug 'jpalardy/vim-slime'
    Plug 'vim-test/vim-test'

    " Saving
    Plug 'lambdalisue/suda.vim'
    Plug '907th/vim-auto-save'

    " Git
    Plug 'lewis6991/gitsigns.nvim'
    Plug 'drzel/vim-repo-edit'
    Plug 'sindrets/diffview.nvim'

    " Registers
    Plug 'svermeulen/vim-subversive'
    Plug 'inkarkat/vim-unconditionalpaste'
    Plug 'chentau/marks.nvim'

    " Movement Commands
    Plug 'andymass/vim-matchup'
    Plug 'chaoren/vim-wordmotion'
    Plug 'junegunn/vim-slash'
    " Plug 'rhysd/clever-f.vim'

    " Normal Commands
    Plug 'terrortylor/nvim-comment'
    Plug 'arthurxavierx/vim-caser'
    Plug 'junegunn/vim-easy-align'
    Plug 'Konfekt/vim-CtrlXA'
    Plug 'tpope/vim-surround'

    " Command Mode
    Plug 'tpope/vim-abolish'
    Plug 'tpope/vim-eunuch'
    Plug 'kazhala/close-buffers.nvim'
    Plug 'tpope/vim-projectionist'

    " Text Objects
    Plug 'tommcdo/vim-ninja-feet'
    Plug 'wellle/targets.vim'
    Plug 'wellle/line-targets.vim'
    Plug 'kana/vim-textobj-user'
    Plug 'kana/vim-textobj-entire'
    Plug 'preservim/vim-textobj-sentence'

    " Language
    Plug 'lervag/vimtex'
    Plug 'JuliaEditorSupport/julia-vim'
    Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
    Plug 'coachshea/vim-textobj-markdown'
    Plug 'fladson/vim-kitty'
    Plug 'anufrievroman/vim-angry-reviewer'

    " UI Stuff
    Plug 'folke/zen-mode.nvim'
    Plug 'akinsho/nvim-bufferline.lua'
    Plug 'lukas-reineke/indent-blankline.nvim',
    Plug 'norcalli/nvim-colorizer.lua'
    Plug 'windwp/windline.nvim'

    " Panels
    Plug 'mbbill/undotree'
    Plug 'folke/trouble.nvim'
    Plug 'folke/todo-comments.nvim'
    Plug 'folke/which-key.nvim'

    " Themes
    Plug 'folke/tokyonight.nvim'

    " LSP
    Plug 'neovim/nvim-lspconfig'
    Plug 'kabouzeid/nvim-lspinstall'
    Plug 'jose-elias-alvarez/null-ls.nvim'
    Plug 'RRethy/vim-illuminate'
    Plug 'ray-x/lsp_signature.nvim'

    " Completion
    Plug 'hrsh7th/nvim-cmp'
    Plug 'hrsh7th/cmp-buffer'
    Plug 'hrsh7th/cmp-path'
    Plug 'hrsh7th/cmp-nvim-lua'
    Plug 'hrsh7th/cmp-nvim-lsp'
    Plug 'tzachar/cmp-tabnine', { 'do': './install.sh' }
    Plug 'L3MON4D3/LuaSnip'
    Plug 'saadparwaiz1/cmp_luasnip'
    Plug 'rafamadriz/friendly-snippets'
    Plug 'windwp/nvim-autopairs'
    Plug 'abecodes/tabout.nvim'

    " Telescope
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'nvim-telescope/telescope-hop.nvim'
    Plug 'nvim-telescope/telescope-symbols.nvim'
    Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make'}
    Plug 'nvim-telescope/telescope-media-files.nvim'
    Plug 'nvim-telescope/telescope-bibtex.nvim'
    Plug 'nvim-telescope/telescope-github.nvim'
    Plug 'crispgm/telescope-heading.nvim'

    " Treesitter
    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    Plug 'nvim-treesitter/nvim-treesitter-textobjects'
    Plug 'nvim-treesitter/playground'
    Plug 'p00f/nvim-ts-rainbow'
    Plug 'code-biscuits/nvim-biscuits'

    " Refactor and Document
    Plug 'danymat/neogen'
    Plug 'ThePrimeagen/refactoring.nvim'
    Plug 'AndrewRadev/splitjoin.vim'

    " Hop, Skip And Jump
    Plug 'IndianBoy42/hop.nvim'
    Plug 'mizlan/iswap.nvim'
    Plug 'mfussenegger/nvim-ts-hint-textobject'
    Plug 'kwkarlwang/bufjump.nvim'

call plug#end()

" !!THEMES!!
highlight link BiscuitColor TSComment
highlight link CleverFDefaultLabel MatchParen
set noshowmode
set termguicolors

let g:tokyonight_style='night'
let g:tokyonight_terminal_colors=v:true
let g:tokyonight_dark_float=v:false
let g:tokyonight_dark_sidebar=v:true
let g:tokyonight_italic_comments=v:true
let g:tokyonight_italic_keywords=v:false
let g:tokyonight_sidebars = [ "qf", "Outline", "terminal", "vim-plug", "undotree", "help", "DiffviewFiles", "DiffviewFileHistory", "juliadoc"]
let g:tokyonight_hide_inactive_statusline=v:true
colorscheme tokyonight

" !!SETTINGS!!
set nrformats-=octal
set clipboard+=unnamedplus
set viminfo='100,f1
set mouse=a
set termguicolors
set hidden
set encoding=UTF-8
set scrolloff=0
set updatetime=100
set backspace=indent,eol,start
set diffopt=internal,filler,closeoff,iwhite,context:100000000
set pumheight=20
set spelllang=en_gb
set lazyredraw

" Search
set ignorecase
set smartcase
set hlsearch
set incsearch hl
set inccommand=split
set gdefault " NOTE : Might break some things
noremap <plug>(slash-after) <cmd>let g:dirJumps='search'<cr>zz

" Indenting
set tabstop=4
set shiftwidth=4
set expandtab

" Word Wrapping
set nowrap
set linebreak
set breakindent
set breakindentopt=shift:2
" set textwidth=100
call matchadd('TabLine', '\%101v', 203) "Colour Column

" Folding
set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()
set foldlevel=20

" Splitting
set splitbelow
set splitright
augroup windowPositioning
    autocmd!
    autocmd FileType help :wincmd H | vertical resize 90<cr>
    autocmd FileType juliadoc wincmd H
    autocmd FileType qf wincmd J
augroup END

set shortmess=Iflmnrwxt

" Line Numbering
set signcolumn=yes:2
set number
set relativenumber

augroup numbertoggle
    autocmd!
    autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu && mode() != "i" | set rnu   | endif
    autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu                  | set nornu | endif
augroup END

set cursorline
augroup DeclutterSleepyWins
    autocmd!
    autocmd BufEnter,FocusGained,InsertLeave,WinEnter * setlocal cursorline
    autocmd BufLeave,FocusLost,InsertEnter,WinLeave * setlocal nocursorline
    autocmd BufEnter,FocusGained,WinEnter * setlocal signcolumn=yes:2
    autocmd BufLeave,FocusLost,WinLeave * setlocal signcolumn=no
augroup END

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

let g:surround_no_mappings = 1
nmap dp  <Plug>Dsurround
nmap cp  <Plug>Csurround
nmap cP  <Plug>CSurround
" nmap yp  <Plug>Ysurround
" nmap yP  <Plug>YSurround
" nmap ypp <Plug>Yssurround
" nmap yPp <Plug>YSsurround
" nmap yPP <Plug>YSsurround

let g:loaded_matchit = 1
let g:matchup_override_vimtex = 1
let g:matchup_matchparen_offscreen = {'method': 'popup'}
" let g:matchup_surround_enabled = 1
" nmap cp <plug>(matchup-cs%)
" nmap dp <plug>(matchup-ds%)

" highlight Yank
augroup LuaHighlight
    autocmd!
    autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank()
augroup END

" " Surround
" " let g:surround_mappings_style="sandwich"
" let g:surround_prefix="g,h"
" let g:surround_load_keymaps=v:false

" Leader key remap
set timeoutlen=500
nnoremap <SPACE> <Nop>
nnoremap <BackSPACE> <Nop>
xnoremap <SPACE> <Nop>
xnoremap <BackSPACE> <Nop>
let mapleader = "\<space>"
let gaplocalleader = "\\"

" Kitty
let g:slime_no_mappings = 1
let g:slime_target = "kitty"
let test#strategy = "kitty"

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

" Kak style always selecting, still needs lots of work
" nnoremap h vh
" nnoremap H vh
" nnoremap j vj
" nnoremap J vj
" nnoremap k vk
" nnoremap K vk
" nnoremap l vl
" nnoremap L vl

" nnoremap w vwh
" nnoremap W vw
" nnoremap b hvb
" nnoremap B vb
" nnoremap e ve
" nnoremap E ve

" xnoremap h <esc>vh
" xnoremap H h
" xnoremap j <esc>vj
" xnoremap J j
" xnoremap k <esc>vk
" xnoremap K k
" xnoremap l <esc>vl
" xnoremap L l

" xnoremap w <esc>vwh
" xnoremap W w
" xnoremap b <esc>hvb
" xnoremap B b
" xnoremap e <esc>ve
" xnoremap E e

" nnoremap <C-j> H
" nnoremap <C-h> M
" nnoremap <C-k> L

" xnoremap <C-j> H
" xnoremap <C-h> M
" xnoremap <C-k> L

" onoremap <C-j> H
" onoremap <C-h> M
" onoremap <C-k> L

" Jump back to where you were
" nnoremap J gi

" Some Sensible Changes
" nnoremap U <c-r>
" nnoremap Y y$
" nnoremap Q @q

" Mappings for left actions
" nnoremap dD d^
" nnoremap yY y^
" nnoremap cC c^

" Make command easier to reach (requires clever-f)
" nnoremap ; :
" nnoremap : <nop>
" nnoremap q; q:
" nnoremap @; @:
" xnoremap ; :
" xnoremap : <nop>
" xnoremap q; q:
" xnoremap @; @:

" Stop The Deselecting
" xnoremap < <gv
" xnoremap > >gv

" Visual Moving
" xnoremap J :move '>+1<cr>gv=gv
" xnoremap K :move '<-2<cr>gv=gv

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

" RSI, without the RSI
" inoremap        <c-a> <C-O>^
" cnoremap        <c-a> <Home>
" inoremap        <c-e> <End>
" cnoremap        <c-e> <End>

" Terminal
tnoremap <Esc> <C-\><C-n>

" Panel Specific Mappings
augroup panelMappings
    autocmd!
    autocmd filetype Outline             map <buffer> o     <cmd>lua require('symbols-outline')._goto_location(true)<cr><cmd>sleep 2<cr><cmd>SymbolsOutlineClose<cr>
    autocmd filetype qf                  map <buffer> <esc> <cmd>q<cr>
    autocmd filetype help                map <buffer> <esc> <cmd>q<cr>
    autocmd filetype vim-plug            map <buffer> <esc> <cmd>q<cr>
    autocmd filetype juliadoc            map <buffer> <esc> <cmd>q<cr>
    " autocmd filetype NeogitStatus      map <buffer> <esc> <cmd>tabclose<cr>
    " autocmd filetype NeogitPopup       map <buffer> <esc> <cmd>q<cr>
    autocmd filetype undotree            map <buffer> <esc> <cmd>UndotreeHide<cr>
    autocmd filetype lspinfo             map <buffer> <esc> <cmd>q<cr>
    autocmd filetype DiffviewFiles       map <buffer> <esc> <cmd>DiffviewClose<cr>
    autocmd filetype DiffviewFileHistory map <buffer> <esc> <cmd>DiffviewClose<cr>
    autocmd filetype tsplayground        map <buffer> <esc> <cmd>q<cr>
augroup END

" Hop, Skip And Jump
" nmap s <cmd>lua require'hop'.hint_char1()<cr>
" nmap S <cmd>ISwapWith<cr>



noremap <silent> £ :silent :exe "let @/='" . expand("<cWORD>") . "'"<cr>

" " Clever-f
" let g:clever_f_across_no_line=1
" let g:clever_f_smart_case=1

nnoremap <m-n> ;
nnoremap <m-N> ,
xnoremap <m-n> ;
xnoremap <m-N> ,

" Word Motion Command
let g:wordmotion_prefix = '$'

" Nvim Comment
nmap <c-_> g,cc
xmap <c-_> g,c

" Arrows

" Unmap Pluggins
let g:kitty_navigator_no_mappings = 1
let g:splitjoin_split_mapping = ''
let g:splitjoin_join_mapping = ''
let g:UnconditionalPaste_no_mappings = 1
let g:caser_no_mappings	= 1
let g:textobj_markdown_no_default_key_mappings=1

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

let g:julia_blocks=0

function! JumpWithinFile(back, forw)
    let [n, i] = [bufnr('%'), 1]
    let p = [n] + getpos('.')[1:]
    sil! exe 'norm!1' . a:forw
    while 1
        let p1 = [bufnr('%')] + getpos('.')[1:]
        if n == p1[0] | break | endif
        if p == p1
            sil! exe 'norm!' . (i-1) . a:back
            break
        endif
        let [p, i] = [p1, i+1]
        sil! exe 'norm!1' . a:forw
    endwhile
endfunction

nnoremap <silent> <c-i> :call JumpWithinFile("\<c-i>", "\<c-o>")<cr>
nnoremap <silent> <c-o> :call JumpWithinFile("\<c-o>", "\<c-i>")<cr>

redraw
