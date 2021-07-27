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
    Plug 'lambdalisue/suda.vim'
    Plug 'nvim-lua/plenary.nvim'
    Plug 'tpope/vim-repeat'
    Plug 'kyazdani42/nvim-web-devicons'
    Plug 'winston0410/cmd-parser.nvim'

    " Project Management
    Plug 'rmagatti/auto-session'
    Plug 'rmagatti/session-lens'
    Plug 'tpope/vim-projectionist'
    Plug 'farmergreg/vim-lastplace'
    Plug '907th/vim-auto-save'
    " Plug 'Pocco81/AutoSave.nvim'

    " Git
    Plug 'TimUntersberger/neogit'
    Plug 'lewis6991/gitsigns.nvim'
    Plug 'drzel/vim-repo-edit'
    Plug 'rhysd/committia.vim'
    Plug 'sindrets/diffview.nvim'

    " Registers
    Plug 'svermeulen/vim-subversive'
    Plug 'inkarkat/vim-unconditionalpaste'

    " Movement Commands
    Plug 'unblevable/quick-scope'
    Plug 'phaazon/hop.nvim'
    " Plug 'ggandor/lightspeed.nvim'
    Plug 'andymass/vim-matchup'
    Plug 'chaoren/vim-wordmotion'
    Plug 'junegunn/vim-slash'

    " Normal Commands
    Plug 'blackCauldron7/surround.nvim'
    Plug 'tommcdo/vim-nowchangethat'
    Plug 'terrortylor/nvim-comment'
    Plug 'arthurxavierx/vim-caser'
    Plug 'junegunn/vim-easy-align'
    Plug 'AndrewRadev/splitjoin.vim'
    Plug 'fvictorio/vim-extract-variable'

    " Command Mode
    Plug 'tpope/vim-abolish'
    Plug 'tpope/vim-eunuch'
    Plug 'nacro90/numb.nvim'
    Plug 'winston0410/range-highlight.nvim'
    Plug 'Asheq/close-buffers.vim'

    " Text Objects
    Plug 'wellle/targets.vim'
    Plug 'wellle/line-targets.vim'
    Plug 'michaeljsmith/vim-indent-object'
    Plug 'tommcdo/vim-ninja-feet'
    " Plug 'junegunn/vim-after-object'
    Plug 'romgrk/equal.operator'

    " Insert Mode
    Plug 'Konfekt/vim-CtrlXA'
    Plug 'tpope/vim-rsi'

    " LANGUAGE
    Plug 'lervag/vimtex'
    Plug 'JuliaEditorSupport/julia-vim'
    Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
    " Plug 'neovimhaskell/haskell-vim'

    " UI Stuff
    Plug 'folke/zen-mode.nvim'
    Plug 'akinsho/nvim-bufferline.lua'
    Plug 'hoob3rt/lualine.nvim'
    Plug 'lewis6991/foldsigns.nvim'
    Plug 'lukas-reineke/indent-blankline.nvim',
    Plug 'norcalli/nvim-colorizer.lua'
    Plug 'sunjon/shade.nvim'
    Plug 'folke/twilight.nvim'

    " Panels
    Plug 'mipmip/panelmanager.vim'
    Plug 'mbbill/undotree'
    Plug 'kyazdani42/nvim-tree.lua'
    Plug 'simrat39/symbols-outline.nvim'
    Plug 'folke/trouble.nvim'
    Plug 'akinsho/nvim-toggleterm.lua'
    Plug 'folke/which-key.nvim'
    Plug 'folke/todo-comments.nvim'
    Plug 'lvim-tech/lvim-helper'
    Plug 'jpalardy/vim-slime'

    " File Manager
    Plug 'tpope/vim-vinegar'

    " Quickfix
    Plug 'kevinhwang91/nvim-bqf'
    Plug 'tommcdo/vim-lister'

    " Themes
    Plug 'folke/tokyonight.nvim'

    " LSP
    Plug 'neovim/nvim-lspconfig'
    Plug 'kabouzeid/nvim-lspinstall'
    Plug 'onsails/lspkind-nvim'
    Plug 'ray-x/lsp_signature.nvim'
    Plug 'folke/lsp-colors.nvim'
    Plug 'RRethy/vim-illuminate'
    Plug 'kosayoda/nvim-lightbulb'
    Plug 'jose-elias-alvarez/null-ls.nvim'

    " Completion
    Plug 'hrsh7th/nvim-compe'
    " Plug 'hrsh7th/vim-vsnip'
    Plug 'L3MON4D3/LuaSnip'
    " Plug 'hrsh7th/vim-vsnip-integ'
    Plug 'rafamadriz/friendly-snippets'
    Plug 'windwp/nvim-autopairs'
    Plug 'tzachar/compe-tabnine', { 'do': './install.sh' }

    " Telescope
    Plug 'nvim-lua/popup.nvim'
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'nvim-telescope/telescope-symbols.nvim'
    Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make'}
    Plug 'nvim-telescope/telescope-media-files.nvim'
    Plug 'nvim-telescope/telescope-bibtex.nvim'
    Plug 'nvim-telescope/telescope-github.nvim'
    Plug 'crispgm/telescope-heading.nvim'
    Plug 'nvim-telescope/telescope-hop.nvim'

    " Treesitter
    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'} 
    Plug 'code-biscuits/nvim-biscuits'
    Plug 'nvim-treesitter/nvim-treesitter-textobjects'
    Plug 'nvim-treesitter/nvim-treesitter-refactor'
    Plug 'p00f/nvim-ts-rainbow'
    Plug 'nvim-treesitter/playground'
    Plug 'RRethy/nvim-treesitter-textsubjects'
    Plug 'mizlan/iswap.nvim'
    Plug 'mfussenegger/nvim-ts-hint-textobject'
    Plug 'abecodes/tabout.nvim'
call plug#end()

" !!THEMES!!
highlight link BiscuitColor TSComment
set noshowmode
set termguicolors

let g:tokyonight_style='night'
let g:tokyonight_terminal_colors=1
let g:tokyonight_sidebars = [ "qf", "Outline", "terminal", "vim-plug", "undotree", "help", "DiffviewFiles", "juliadoc"]
let g:tokyonight_hide_inactive_statusline=1
colorscheme tokyonight

" " Indent Blankline Settings
let g:indent_blankline_char_list = ['│']
let g:indent_blankline_char_highlight_list = ['rainbowcol7', 'rainbowcol6', 'rainbowcol5', 'rainbowcol4', 'rainbowcol3', 'rainbowcol2', 'rainbowcol1']

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
call matchadd('TabLine', '\%101v', 203)

" Folding
set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()
set foldlevel=20

" Splitting
set splitbelow
set splitright
augroup windowPositioning
  autocmd FileType help wincmd H
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
    nmap <buffer><C-n> <Plug>(committia-scroll-diff-down-half)
    nmap <buffer><C-p> <Plug>(committia-scroll-diff-up-half)
    imap <buffer><C-n> <Plug>(committia-scroll-diff-down-half)
    imap <buffer><C-p> <Plug>(committia-scroll-diff-up-half)
endfunction

" Wellle Tagets settings
autocmd User targets#mappings#user call targets#mappings#extend({
    \ 'A': {'argument': [{'o': '[{([]', 'c': '[])}]', 's': '[,;]'}]},
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

lua << EOF
local wk = require("which-key")
wk.setup {
    plugins = {
        marks = true,
        registers = true,
        spelling = {
            enabled = true,
            suggestions = 20,
        },
        presets = {
            operators = true,
            motions = true, 
            text_objects = true, 
            windows = true, 
            nav = true, 
            z = true, 
            g = true, 
        },
    },
    operators = { gc = "Comments" },
    icons = {
        breadcrumb = "»", 
        separator = "➜", 
        group = "+", 
    },
    window = {
        border = "none", 
        position = "bottom", 
        margin = { 1, 0, 1, 0 }, 
        padding = { 2, 2, 2, 2 }, 
    },
    layout = {
        height = { min = 4, max = 30 }, 
        width = { min = 20, max = 50 }, 
        spacing = 3, 
    },
    hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ "}, 
    show_help = true 
}
EOF

" !!MAPPINGS!!
nnoremap <expr> j v:count ? (v:count > 5 ? "m'" . v:count : '') . 'j' : 'gj'
nnoremap <expr> k v:count ? (v:count > 5 ? "m'" . v:count : '') . 'k' : 'gk'

xnoremap <expr> j v:count ? (v:count > 5 ? "m'" . v:count : '') . 'j' : 'gj'
xnoremap <expr> k v:count ? (v:count > 5 ? "m'" . v:count : '') . 'k' : 'gk'

onoremap <expr> j v:count ? (v:count > 5 ? "m'" . v:count : '') . 'j' : 'gj'
onoremap <expr> k v:count ? (v:count > 5 ? "m'" . v:count : '') . 'k' : 'gk'

nnoremap <expr> H getline('.')[0 : col('.') - 2] =~# '^\s\+$' ? '0' : '^'
xnoremap <expr> H getline('.')[0 : col('.') - 2] =~# '^\s\+$' ? '0' : '^'
onoremap <expr> H getline('.')[0 : col('.') - 2] =~# '^\s\+$' ? '0' : '^'

nnoremap <expr> L getline('.')[col('.') : -1] =~# '^\s\+$' ? '$' : 'g_'
xnoremap <expr> L getline('.')[col('.') : -1] =~# '^\s\+$' ? '$' : 'g_'
onoremap <expr> L getline('.')[col('.') : -1] =~# '^\s\+$' ? '$' : 'g_'

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
onoremap ' `
xnoremap ` '
xnoremap ' `

" Stop The Deselecting
vnoremap < <gv
vnoremap > >gv


" Insert Mode
inoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
inoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')

" imap <silent><expr> <Tab> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<Tab>' 
" inoremap <silent> <S-Tab> <cmd>lua require'luasnip'.jump(-1)<Cr>
" imap <silent><expr> <C-E> luasnip#choice_active() ? '<Plug>luasnip-next-choice' : '<C-E>'

inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })
inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })
" inoremap <silent><expr> <C-e>     compe#close('<C-e>')
" inoremap <silent><expr> <C-Space> compe#complete()

"
" Terminal
tnoremap <Esc> <C-\><C-n>

" Panel Specific Mappings
augroup panelMappings
  au filetype Outline map <buffer> o <cmd>lua require('symbols-outline')._goto_location(true)<cr><cmd>sleep 2<cr><cmd>SymbolsOutlineClose<cr>
  au filetype qf map <buffer> <esc> <cmd>q<cr>
  au filetype help map <buffer> <esc> <cmd>q<cr>
  au filetype vim-plug map <buffer> <esc> <cmd>q<cr>
  au filetype juliadoc map <buffer> <esc> <cmd>q<cr>
  au filetype LvimHelper map <buffer> <esc> <cmd>q<cr>
  au filetype NeogitStatus map <buffer> <esc> <cmd>tabclose<cr>
  au filetype NeogitPopup map <buffer> <esc> <cmd>q<cr>
  au filetype toggleterm map <buffer> <esc> <cmd>ToggleTermCloseAll<cr>
augroup END

" Vim Hop, ISwap and TS hint object
nmap s <cmd>lua require'hop'.hint_char1()<cr>
omap ss <cmd>lua require'hop'.hint_char1()<cr>
vmap ss <cmd>lua require'hop'.hint_char1()<cr>
nmap S <cmd>ISwapWith<cr>
omap <silent> S :<C-U>lua require('tsht').nodes()<CR>
vmap <silent> S :lua require('tsht').nodes()<CR>

" Word Motion Command
let g:wordmotion_prefix = '$'

" Text Object Mappings

onoremap <silent>Ai :<C-u>cal <Sid>HandleTextObjectMapping(0, 1, 0, [line("."), line("."), col("."), col(".")])<CR>
onoremap <silent>Ii :<C-u>cal <Sid>HandleTextObjectMapping(1, 1, 0, [line("."), line("."), col("."), col(".")])<CR>
vnoremap <silent>Ai :<C-u>cal <Sid>HandleTextObjectMapping(0, 1, 1, [line("'<"), line("'>"), col("'<"), col("'>")])<CR><Esc>gv
vnoremap <silent>Ii :<C-u>cal <Sid>HandleTextObjectMapping(1, 1, 1, [line("'<"), line("'>"), col("'<"), col("'>")])<CR><Esc>gv

let equal_operator_default_mappings = 0
omap a= <Plug>(operator-rhs)
omap i= <Plug>(operator-lhs)
vmap a= <Plug>(operator-rhs)
vmap i= <Plug>(operator-lhs)

" Nvim Comment
lua require('nvim_comment').setup({comment_empty = false})
nmap <c-_> gcc
xmap <c-_> gc

" Arrows
nmap <silent> <right>   <cmd>BufferLineCycleNext<cr>
nmap <silent> <left>    <cmd>BufferLineCyclePrev<cr>
nmap <silent> <C-right> <cmd>tabnext<cr>
nmap <silent> <C-left>  <cmd>tabprevious<cr>
nmap <silent> <S-right> <cmd>tabnext<cr>
nmap <silent> <S-left>  <cmd>tabprevious<cr>
nmap <silent> <C-up>    <cmd>try <bar> cnext     <bar> catch /E553/ <bar> cfirst <bar> endtry<CR>
nmap <silent> <C-down>  <cmd>try <bar> cprevious <bar> catch /E553/ <bar> clast  <bar> endtry<CR>
nmap <silent> <S-up>    <cmd>try <bar> lnext     <bar> catch /E553/ <bar> lfirst <bar> endtry<CR>
nmap <silent> <S-down>  <cmd>try <bar> lprevious <bar> catch /E553/ <bar> llast  <bar> endtry<CR>

" Unmap Pluggins
let g:splitjoin_split_mapping = ''
let g:splitjoin_join_mapping = ''
let g:UnconditionalPaste_no_mappings = 1
let g:caser_no_mappings	= 1

"List Clearing mappings
command! CClear cexpr []
command! LClear lexpr []

" Panels

" REGISTER PLUGIN AND THEIR PANEL POSITION
call panelmanager#init()

"                         POSITION  IDENTIFIER          OPEN COMMAND                            CLOSE COMMAND
call  PMRegisterPanelView('left',   'undotree',         'UndotreeShow',                         'UndotreeHide')
call  PMRegisterPanelView('left',   'nvim-tree',        'NvimTreeOpen',                         'NvimTreeClose')
call  PMRegisterPanelView('bottom', 'quickfix',         'Trouble quickfix',                     'TroubleClose')
call  PMRegisterPanelView('bottom', 'loclist',          'Trouble loclist',                      'TroubleClose')
call  PMRegisterPanelView('bottom', 'errorlistdoc',     'Trouble lsp_document_diagnostics',     'TroubleClose')
call  PMRegisterPanelView('bottom', 'errorlist',        'Trouble lsp_workspace_diagnostics',    'TroubleClose')
call  PMRegisterPanelView('bottom', 'troubleTelescope', 'Trouble telescope',                    'TroubleClose')
call  PMRegisterPanelView('bottom', 'Todo-Trouble',     'TodoTrouble',                          'TroubleClose')
call  PMRegisterPanelView('bottom', 'loclistFilter',    'lopen',                                'windo if &buftype == "quickfix" || &buftype == "locationlist" | q | endif')
call  PMRegisterPanelView('bottom', 'quickfixFilter',   'copen',                                'windo if &buftype == "quickfix" || &buftype == "locationlist" | q | endif')
call  PMRegisterPanelView('bottom', 'term',             'ToggleTermOpenAll',                    'ToggleTermCloseAll')
call  PMRegisterPanelView('right',  'symbols',          'SymbolsOutlineOpen',                   'SymbolsOutlineClose')
call  PMRegisterPanelView('top',    'gitdiff',          'DiffviewOpen',                         'DiffviewClose')

function CloseAllPanels()
  UndotreeHide
  windo if &buftype == "quickfix" || &buftype == "locationlist" | q | endif
  " windo if &filetype == "toggleterm" | q | endif
  ToggleTermCloseAll
  NvimTreeClose
  TroubleClose
  TroubleClose 
  SymbolsOutlineClose
  DiffviewClose
  windo if &filetype == "help" | q | endif
  windo if &filetype == "LvimHelper" | q | endif
  windo if &filetype == "juliadoc" | q | endif
endfunction


lua << EOF
require('config_main')
require('config_ui')
require('config_panels')
require('config_bindings')
EOF

silent! !git rev-parse --is-inside-work-tree
if v:shell_error == 0
  lua require("which-key").register({["<leader>f"]={f = {"<cmd>Telescope git_files<cr>", "Find Files"}}})
else
  lua require("which-key").register({["<leader>f"]={f = {"<cmd>Telescope fd<cr>", "Find Files"}}})
endif


" let g:haskell_enable_quantification = 1
" let g:haskell_enable_recursivedo = 1
" let g:haskell_enable_arrowsyntax = 1
" let g:haskell_enable_pattern_synonyms = 1
" let g:haskell_enable_typeroles = 1
" let g:haskell_enable_static_pointers = 1
" let g:haskell_backpack = 1

redraw
