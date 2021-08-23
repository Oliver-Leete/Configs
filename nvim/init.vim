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
    Plug 'winston0410/cmd-parser.nvim'

    " The Glory
    Plug 'alexherbo2/kakoune.vim'

    " Saving
    Plug 'lambdalisue/suda.vim'
    Plug '907th/vim-auto-save'
    Plug 'jghauser/mkdir.nvim'

    " Project Management
    Plug 'tpope/vim-projectionist'

    " Git
    Plug 'TimUntersberger/neogit'
    Plug 'lewis6991/gitsigns.nvim'
    Plug 'drzel/vim-repo-edit'
    Plug 'rhysd/committia.vim'
    Plug 'sindrets/diffview.nvim'

    " Registers
    Plug 'svermeulen/vim-subversive'
    Plug 'inkarkat/vim-unconditionalpaste'
    Plug 'AckslD/nvim-neoclip.lua'

    " Movement Commands
    " Plug 'unblevable/quick-scope'
    Plug 'andymass/vim-matchup'
    Plug 'chaoren/vim-wordmotion'
    Plug 'junegunn/vim-slash'
    Plug 'caenrique/swap-buffers.nvim'

    " Normal Commands
    Plug 'blackCauldron7/surround.nvim'
    Plug 'terrortylor/nvim-comment'
    Plug 'arthurxavierx/vim-caser'
    Plug 'junegunn/vim-easy-align'
    Plug 'Konfekt/vim-CtrlXA'
    Plug 'rhysd/clever-f.vim'

    " Refactoring
    Plug 'AndrewRadev/splitjoin.vim'
    Plug 'fvictorio/vim-extract-variable'

    " Command Mode
    Plug 'tpope/vim-abolish'
    Plug 'tpope/vim-eunuch'
    Plug 'nacro90/numb.nvim'
    Plug 'winston0410/range-highlight.nvim'
    Plug 'kazhala/close-buffers.nvim'

    " Text Objects
    Plug 'tommcdo/vim-ninja-feet'
    Plug 'wellle/targets.vim'
    Plug 'wellle/line-targets.vim'
    " Plug 'michaeljsmith/vim-indent-object'
    Plug 'kana/vim-textobj-user'
    Plug 'kana/vim-textobj-entire'

    " Language
    Plug 'lervag/vimtex'
    Plug 'JuliaEditorSupport/julia-vim'
    Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
    Plug 'coachshea/vim-textobj-markdown'
    Plug 'fladson/vim-kitty'

    " UI Stuff
    Plug 'folke/zen-mode.nvim'
    Plug 'akinsho/nvim-bufferline.lua'
    Plug 'hoob3rt/lualine.nvim'
    Plug 'lewis6991/foldsigns.nvim'
    Plug 'lukas-reineke/indent-blankline.nvim',
    Plug 'norcalli/nvim-colorizer.lua'
    Plug 'folke/twilight.nvim'

    " Panels
    " Plug 'mipmip/panelmanager.vim'
    Plug 'mbbill/undotree'
    Plug 'kyazdani42/nvim-tree.lua'
    Plug 'simrat39/symbols-outline.nvim'
    Plug 'folke/trouble.nvim'
    Plug 'folke/which-key.nvim'
    Plug 'folke/todo-comments.nvim'
    Plug 'lvim-tech/lvim-helper'
    Plug 'jpalardy/vim-slime'
    Plug 'kevinhwang91/nvim-bqf'

    " Terminal Stuff
    Plug 'akinsho/nvim-toggleterm.lua'
    Plug 'michaelb/sniprun', {'do': 'bash install.sh 1'}
    Plug 'dccsillag/magma-nvim', {'do': ':UpdateRemotePlugins'}

    " Themes
    Plug 'folke/tokyonight.nvim'

    " LSP
    Plug 'neovim/nvim-lspconfig'
    Plug 'kabouzeid/nvim-lspinstall'
    Plug 'jose-elias-alvarez/null-ls.nvim'
    Plug 'RRethy/vim-illuminate'
    Plug 'ray-x/lsp_signature.nvim'
    Plug 'onsails/lspkind-nvim'
    Plug 'kosayoda/nvim-lightbulb'

    " Completion
    Plug 'hrsh7th/nvim-compe'
    Plug 'L3MON4D3/LuaSnip'
    Plug 'rafamadriz/friendly-snippets'
    Plug 'windwp/nvim-autopairs'
    Plug 'tzachar/compe-tabnine', { 'do': './install.sh' }
    Plug 'abecodes/tabout.nvim'

    " Telescope
    Plug 'nvim-lua/popup.nvim'
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'nvim-telescope/telescope-symbols.nvim'
    Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make'}
    Plug 'nvim-telescope/telescope-media-files.nvim'
    Plug 'nvim-telescope/telescope-bibtex.nvim'
    Plug 'nvim-telescope/telescope-github.nvim'
    Plug 'crispgm/telescope-heading.nvim'


    " Treesitter
    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    Plug 'nvim-treesitter/nvim-treesitter-textobjects'
    Plug 'nvim-treesitter/nvim-treesitter-refactor'
    Plug 'nvim-treesitter/playground'
    Plug 'p00f/nvim-ts-rainbow'
    Plug 'code-biscuits/nvim-biscuits'
    Plug 'RRethy/nvim-treesitter-textsubjects'
    Plug 'SmiteshP/nvim-gps'

    " Hop, Skip And Jump
    Plug 'phaazon/hop.nvim'
    Plug 'nvim-telescope/telescope-hop.nvim'
    Plug 'mizlan/iswap.nvim'
    Plug 'mfussenegger/nvim-ts-hint-textobject'
    
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
let g:tokyonight_sidebars = [ "qf", "Outline", "terminal", "vim-plug", "undotree", "help", "DiffviewFiles", "juliadoc"]
let g:tokyonight_hide_inactive_statusline=v:true
colorscheme tokyonight


" Vim Slime Settings
let g:slime_target = "neovim"

" !!SETTINGS!!
set nrformats-=octal
set clipboard+=unnamedplus
set viminfo='100,f1
set mouse=a
set termguicolors
set hidden
set encoding=UTF-8
set scrolloff=3
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
noremap <plug>(slash-after) zz

" Indenting
set tabstop=4
set shiftwidth=4
set expandtab

" Word Wrapping
set linebreak
set breakindent
set breakindentopt=shift:2
call matchadd('TabLine', '\%101v', 203) "Colour Column

" Folding
set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()
set foldlevel=20

" Splitting
set splitbelow
set splitright
augroup windowPositioning
  autocmd FileType help :wincmd H | vertical resize 90<cr>
  autocmd FileType juliadoc wincmd H
  " autocmd FileType gitcommit wincmd H
  autocmd FileType qf wincmd J
augroup END
" autocmd VimResized * exe "normal \<c-w>="

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
augroup ActiveWindowBufferCursorline
    autocmd!
    autocmd WinEnter * setlocal cursorline
    autocmd BufEnter * setlocal cursorline
    autocmd WinLeave * setlocal nocursorline
    autocmd BufLeave * setlocal nocursorline
augroup END

" Saving and Backup
set confirm
set noswapfile
set undodir=~/.vim/undo//
set undofile
let g:auto_save = 1  " enable AutoSave on Vim startup
let g:auto_save_silent = 1  " do not display the auto-save notification


" Git Committia Settings
let g:committia_hooks = {}
function! g:committia_hooks.edit_open(info)
    " Additional settings
    setlocal spell

    " If no commit message, start with insert mode
    if a:info.vcs ==# 'git' && getline(1) ==# ''
        startinsert
    endif

    " Scroll the diff window from insert mode
    " Map <C-n> and <C-p>
    nmap <buffer><C-down> <Plug>(committia-scroll-diff-down-half)
    nmap <buffer><C-up> <Plug>(committia-scroll-diff-up-half)
    imap <buffer><C-down> <Plug>(committia-scroll-diff-down-half)
    imap <buffer><C-up> <Plug>(committia-scroll-diff-up-half)
endfunction

" Wellle Tagets settings
autocmd User targets#mappings#user call targets#mappings#extend({
    \ 'a': {'argument': [{'o': '[{([]', 'c': '[])}]', 's': '[,;]'}]},
    \ 'x': {'line': [{'c': 1}]},
    \ '$': {},
    \ })
let g:targets_seekRanges = 'cc cr cb cB lc ac Ac lr rr ll lb ar ab lB Ar aB Ab AB rb rB al Al'
" let g:targets_nl = 'nl'
let g:targets_gracious = 1
let targets_nl = 'nN'

let g:loaded_matchit = 1
let g:matchup_override_vimtex = 1
let g:matchup_matchparen_offscreen = {'method': 'popup'}

" highlight Yank
augroup LuaHighlight
  autocmd!
    autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank()
augroup END

" Surround
let g:surround_mappings_style="surround"

" Leader key remap
set timeoutlen=500
nnoremap <SPACE> <Nop>
nnoremap <BackSPACE> <Nop>
xnoremap <SPACE> <Nop>
xnoremap <BackSPACE> <Nop>
let mapleader = "\<space>"
let maplocalleader = "\\"

" !!MAPPINGS!!

nnoremap <C-j> H
nnoremap <C-h> M
nnoremap <C-k> L

xnoremap <C-j> H
xnoremap <C-h> M
xnoremap <C-k> L

onoremap <C-j> H
onoremap <C-h> M
onoremap <C-k> L

" Jump back to where you were
nnoremap J gi

" Some Sensible Changes
nnoremap U <c-r>
nnoremap Y y$
nnoremap Q @q

" Mappings for left actions
nnoremap dD d^
nnoremap yY y^
nnoremap cC c^

" Make Marks Easier To Reach
nnoremap ` '
nnoremap ' `
onoremap ` '
onoremap ' `
xnoremap ` '
xnoremap ' `

" Make command easier to reach (requires clever-f)
nnoremap ; :
xnoremap ; :
nnoremap : <nop>
xnoremap : <nop>
nnoremap q; q:
xnoremap q; q:
nnoremap @; @:
xnoremap @; @:

" Stop The Deselecting
xnoremap < <gv
xnoremap > >gv

" Visual Moving
xnoremap J :move '>+1<cr>gv=gv
xnoremap K :move '<-2<cr>gv=gv

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

imap <c-g> <plug>(matchup-c_g%)
inoremap <c-s> <cmd>lua require('lsp_signature').toggle_float_win()<CR>

" RSI, without the RSI
inoremap        <c-a> <C-O>^
cnoremap        <c-a> <Home>
inoremap        <c-e> <End>
cnoremap        <c-e> <End>

" Terminal
tnoremap <Esc> <C-\><C-n>

" Panel Specific Mappings
augroup panelMappings
    autocmd filetype Outline       map <buffer> o     <cmd>lua require('symbols-outline')._goto_location(true)<cr><cmd>sleep 2<cr><cmd>SymbolsOutlineClose<cr>
    autocmd filetype qf            map <buffer> <esc> <cmd>q<cr>
    autocmd filetype help          map <buffer> <esc> <cmd>q<cr>
    autocmd filetype vim-plug      map <buffer> <esc> <cmd>q<cr>
    autocmd filetype juliadoc      map <buffer> <esc> <cmd>q<cr>
    autocmd filetype LvimHelper    map <buffer> <esc> <cmd>q<cr>
    autocmd filetype NeogitStatus  map <buffer> <esc> <cmd>tabclose<cr>
    autocmd filetype NeogitPopup   map <buffer> <esc> <cmd>q<cr>
    autocmd filetype toggleterm    map <buffer> <esc> <cmd>ToggleTermCloseAll<cr>
    autocmd filetype undotree      map <buffer> <esc> <cmd>UndotreeHide<cr>
    autocmd filetype lspinfo       map <buffer> <esc> <cmd>q<cr>
    autocmd filetype DiffviewFiles map <buffer> <esc> <cmd>DiffviewClose<cr>
    autocmd filetype tsplayground  map <buffer> <esc> <cmd>q<cr>
augroup END

" Hop, Skip And Jump
nmap s <cmd>lua require'hop'.hint_char1()<cr>
nmap S <cmd>ISwapWith<cr>

omap <silent> S :<C-U>lua require('tsht').nodes()<CR>

xmap <silent> S :lua require('tsht').nodes()<CR>

" Clever-f
let g:clever_f_across_no_line=1
let g:clever_f_smart_case=1

" Word Motion Command
let g:wordmotion_prefix = '$'

" " Text Object Mappings
" onoremap <silent>Ai :<C-u>cal <Sid>HandleTextObjectMapping(0,1,0,[line("."),line("."),col("."),col(".")])<CR>
" onoremap <silent>Ii :<C-u>cal <Sid>HandleTextObjectMapping(1,1,0,[line("."),line("."),col("."),col(".")])<CR>
" xnoremap <silent>Ai :<C-u>cal <Sid>HandleTextObjectMapping(0,1,1,[line("'<"),line("'>"),col("'<"),col("'>")])<CR><Esc>gv
" xnoremap <silent>Ii :<C-u>cal <Sid>HandleTextObjectMapping(1,1,1,[line("'<"),line("'>"),col("'<"),col("'>")])<CR><Esc>gv

" Nvim Comment
nmap <c-_> gcc
xmap <c-_> gc

" Arrows
nmap <silent> <right>   <cmd>BufferLineCycleNext<cr>
nmap <silent> <left>    <cmd>BufferLineCyclePrev<cr>
nmap <silent> <C-right> <cmd>tabnext<cr>
nmap <silent> <C-left>  <cmd>tabprevious<cr>
nmap <silent> <S-right> <cmd>tabnext<cr>
nmap <silent> <S-left>  <cmd>tabprevious<cr>
nmap <silent> <C-down>  <cmd>try<bar>cnext<bar>catch/E553/<bar>cfirst<bar>endtry<CR>
nmap <silent> <C-up>    <cmd>try<bar>cprevious<bar>catch/E553/<bar>clast<bar>endtry<CR>
nmap <silent> <S-down>  <cmd>try<bar>lnext<bar>catch/E553/<bar>lfirst<bar>endtry<CR>
nmap <silent> <S-up>    <cmd>try<bar>lprevious<bar>catch/E553/<bar>llast<bar>endtry<CR>

" Unmap Pluggins
let g:splitjoin_split_mapping = ''
let g:splitjoin_join_mapping = ''
let g:UnconditionalPaste_no_mappings = 1
let g:caser_no_mappings	= 1
let g:textobj_markdown_no_default_key_mappings=1

"List Clearing mappings
command! CClear cexpr[]
command! LClear lexpr[]

" Set Filetypes
au BufNewFile,BufRead *.fish set filetype=fish
au BufNewFile,BufRead *.jl set filetype=julia

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
require('main_config')
require('compleation_config')
require('ui_config')
require('panels_config')
require('n_bindings_config')
require('x_bindings_config')
require('o_bindings_config')
require('i_bindings_config')
require('myfuncs_config')
require('lsp_config')
require('treesitter_config')
require('telescope_config')
require('git_config')
require('snippets_config')
EOF

let g:julia_blocks=0

redraw
