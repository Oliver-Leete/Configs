let $SHELL = "/bin/zsh"
set shell=/bin/zsh

call plug#begin('~/.config/nvim/pluged')
  Plug 'lambdalisue/suda.vim'
  Plug '907th/vim-auto-save'
  Plug 'farmergreg/vim-lastplace'
  Plug 'nvim-lua/plenary.nvim'

  " Project Managment
  Plug 'rmagatti/auto-session'
  Plug 'rmagatti/session-lens'
  Plug 'tpope/vim-projectionist'

  " Git
  Plug 'TimUntersberger/neogit'
  Plug 'lewis6991/gitsigns.nvim'
  Plug 'junegunn/gv.vim'
  Plug 'drzel/vim-repo-edit'
  Plug 'rhysd/committia.vim'
  Plug 'sindrets/diffview.nvim'

  " Registers
  Plug 'svermeulen/vim-subversive'

  " Movement Commands
  Plug 'unblevable/quick-scope'
  Plug 'phaazon/hop.nvim'
  Plug 'andymass/vim-matchup'
  Plug 'chaoren/vim-wordmotion'
  Plug 'junegunn/vim-slash'

  " Editor Commands
  Plug 'arthurxavierx/vim-caser'
  Plug 'inkarkat/vim-unconditionalpaste'
  Plug 'tommcdo/vim-nowchangethat'
  Plug 'tpope/vim-surround'
  Plug 'tpope/vim-repeat'
  Plug 'tommcdo/vim-nowchangethat'
  Plug 'wincent/scalpel'
  Plug 'terrortylor/nvim-comment'
  Plug 'junegunn/vim-easy-align'
  Plug 'AndrewRadev/splitjoin.vim'
  Plug 'tpope/vim-abolish'
  Plug 'tpope/vim-eunuch'
  Plug 'fvictorio/vim-extract-variable'
  Plug 'folke/todo-comments.nvim'
  Plug 'kkoomen/vim-doge', { 'do': { -> doge#install() } }

  " Text Objects
  Plug 'wellle/targets.vim'
  Plug 'wellle/line-targets.vim'
  Plug 'michaeljsmith/vim-indent-object'
  Plug 'coderifous/textobj-word-column.vim'
  Plug 'tommcdo/vim-ninja-feet'
  Plug 'junegunn/vim-after-object'

  " Insert Mode
  Plug 'Konfekt/vim-CtrlXA'
  Plug 'tpope/vim-rsi'

  " LANGUAGE
  Plug 'lervag/vimtex'
  " Plug 'JuliaEditorSupport/julia-vim'
  Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }}

  " UI Stuff
  Plug 'folke/zen-mode.nvim'
  Plug 'hoob3rt/lualine.nvim'
  Plug 'arkav/lualine-lsp-progress'
  Plug 'kyazdani42/nvim-web-devicons'
  Plug 'nacro90/numb.nvim'
  Plug 'winston0410/range-highlight.nvim'
  Plug 'Jorengarenar/vim-syntaxMarkerFold'
  Plug 'akinsho/nvim-bufferline.lua'
  Plug 'Asheq/close-buffers.vim'
  Plug 'lewis6991/foldsigns.nvim'

  " Panels
  Plug 'mipmip/panelmanager.vim'
  Plug 'mbbill/undotree'
  Plug 'kyazdani42/nvim-tree.lua'
  Plug 'simrat39/symbols-outline.nvim'
  Plug 'folke/trouble.nvim'
  Plug 'akinsho/nvim-toggleterm.lua'
  Plug 'folke/which-key.nvim'

  " File Manager
  Plug 'tpope/vim-vinegar'

  " Quickfix
  Plug 'kevinhwang91/nvim-bqf'
  Plug 'tommcdo/vim-lister'

  " Themes
  Plug 'phanviet/vim-monokai-pro'
  Plug 'sainnhe/sonokai'
  Plug 'tanvirtin/monokai.nvim'
  Plug 'folke/tokyonight.nvim'
  Plug 'marko-cerovac/material.nvim'

  " LSP
  Plug 'neovim/nvim-lspconfig'
  Plug 'kabouzeid/nvim-lspinstall'
  Plug 'onsails/lspkind-nvim'
  Plug 'glepnir/lspsaga.nvim'
  Plug 'jubnzv/virtual-types.nvim'
  Plug 'ray-x/lsp_signature.nvim'
  Plug 'folke/lsp-colors.nvim'
  Plug 'RRethy/vim-illuminate'

  " Compleation
  Plug 'hrsh7th/nvim-compe'
  Plug 'hrsh7th/vim-vsnip'
  Plug 'hrsh7th/vim-vsnip-integ'
  Plug 'rafamadriz/friendly-snippets'
  Plug 'windwp/nvim-autopairs'

  " Telescope
  Plug 'nvim-lua/popup.nvim'
  Plug 'nvim-telescope/telescope.nvim'
  Plug 'nvim-telescope/telescope-symbols.nvim'
  Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make'}
  Plug 'nvim-telescope/telescope-media-files.nvim'
  Plug 'tkmpypy/telescope-jumps.nvim'
  Plug 'nvim-telescope/telescope-bibtex.nvim'
  Plug 'nvim-telescope/telescope-github.nvim'

  " Treesitter
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}  " We recommend updating the parsers on update
  Plug 'romgrk/nvim-treesitter-context'
  Plug 'code-biscuits/nvim-biscuits'
  Plug 'nvim-treesitter/nvim-treesitter-textobjects'
  Plug 'nvim-treesitter/nvim-treesitter-refactor'
  Plug 'p00f/nvim-ts-rainbow'
  Plug 'JoosepAlviste/nvim-ts-context-commentstring'

call plug#end()

set errorformat=%tRROR:\ %m\ at\ %f:%l,%-G%.%#
set makeprg=julia\ -e\ \'using\ Pkg;\ Pkg.precompile()\'

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


" Search
set ignorecase
set smartcase
set hlsearch
set incsearch hl
set inccommand=split
noremap <plug>(slash-after) zz

" Indenting
set tabstop=2
set shiftwidth=2
set expandtab

" Word Wrapping
set linebreak
set breakindent
set breakindentopt=shift:2
call matchadd('TabLine', '\%81v', 203)

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
  autocmd FileType gitcommit wincmd H
  autocmd FileType qf wincmd J
augroup END
" autocmd VimResized * exe "normal \<c-w>="

set shortmess=Iflmnrwxt

" Line Numbering
set signcolumn=yes:1
set number
set relativenumber
function! Togglenumbertoggle()
    if !exists('#TestAutoGroup#BufEnter')
        augroup numbertoggle
            autocmd!
            autocmd BufEnter,FocusGained,InsertLeave,WinEnter * if &nu && mode() != "i" | set rnu   | endif
            autocmd BufLeave,FocusLost,InsertEnter,WinLeave   * if &nu                  | set nornu | endif
        augroup END
    else
        augroup numbertoggle
            autocmd!
        augroup END
    endif
endfunction
call Togglenumbertoggle()

" Saving and Backup
set noswapfile
set undodir=~/.vim/undo//
set undofile
let g:auto_save = 1  " enable AutoSave on Vim startup
let g:auto_save_silent = 1  " do not display the auto-save notification

" !!THEMES!!
set cursorline

let g:tokyonight_style='night'
let g:tokyonight_terminal_colors=1
let g:tokyonight_sidebars = [ "qf", "Outline", "terminal", "vim-plug", "undotree", "help", "DiffviewFiles", "juliadoc"]
colorscheme tokyonight

" let g:material_style = 'darker'
" colorscheme material

highlight clear LineNr
highlight clear SignColumn
highlight link BiscuitColor TSComment
set noshowmode
set termguicolors

" Theme Changes
highlight BufferLineFill guifg=#0e0e14 guibg=#0e0e14

highlight link LspSagaHoverBorder            FloatBorder
highlight link LspSagaSignatureHelpBorder    FloatBorder
highlight link LspSagaCodeActionBorder       FloatBorder
highlight link LspSagaDefPreviewBorder       FloatBorder
highlight link LspSagaDiagnosticBorder       FloatBorder
highlight link LspSagaShTruncateLine         FloatBorder
highlight link LspSagaDocTruncateLine        FloatBorder
highlight link LspSagaCodeActionTruncateLine FloatBorder
highlight link LspSagaProviderTruncateLine   FloatBorder
highlight link LspSagaDiagnosticTruncateLine FloatBorder
highlight link LspSagaBorderTitle            FloatBorder
highlight link TelescopePromptBorder         FloatBorder
highlight link TelescopePreviewBorder        FloatBorder
highlight link TelescopeResultsBorder        FloatBorder
highlight link BqfPreveiwBorder              FloatBorder

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
    imap <buffer><C-n> <Plug>(committia-scroll-diff-down-half)
    imap <buffer><C-p> <Plug>(committia-scroll-diff-up-half)
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

autocmd VimEnter * call after_object#enable(['a', 'af'], '=', ':', '-', '#', ' ')

let g:loaded_matchit = 1
let g:matchup_override_vimtex = 1
let g:matchup_matchparen_offscreen = {'method': 'popup'}

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
let maplocalleader = "\\"

lua << EOF
local wk = require("which-key")
wk.setup {
  plugins = {
    marks = true, -- shows a list of your marks on ' and `
    registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
    -- the presets plugin, adds help for a bunch of default keybindings in Neovim
    -- No actual key bindings are created
    presets = {
      operators = true, -- adds help for operators like d, y, ... and registers them for motion / text object completion
      motions = true, -- adds help for motions
      text_objects = true, -- help for text objects triggered after entering an operator
      windows = true, -- default bindings on <c-w>
      nav = true, -- misc bindings to work with windows
      z = true, -- bindings for folds, spelling and others prefixed with z
      g = true, -- bindings for prefixed with g
    },
  },
  -- add operators that will trigger motion and text object completion
  -- to enable all native operators, set the preset / operators plugin above
  operators = { gc = "Comments" },
  icons = {
    breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
    separator = "➜", -- symbol used between a key and it's label
    group = "+", -- symbol prepended to a group
  },
  window = {
    border = "none", -- none, single, double, shadow
    position = "bottom", -- bottom, top
    margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
    padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
  },
  layout = {
    height = { min = 4, max = 30 }, -- min and max height of the columns
    width = { min = 20, max = 50 }, -- min and max width of the columns
    spacing = 3, -- spacing between columns
  },
  hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ "}, -- hide mapping boilerplate
  show_help = true -- show help message on the command line when the popup is visible
}
EOF

" !!MAPPINGS!!
nnoremap <expr> j v:count ? (v:count > 5 ? "m'" . v:count : '') . 'j' : 'gj'
nnoremap <expr> k v:count ? (v:count > 5 ? "m'" . v:count : '') . 'k' : 'gk'

xnoremap <expr> j v:count ? (v:count > 5 ? "m'" . v:count : '') . 'j' : 'gj'
xnoremap <expr> k v:count ? (v:count > 5 ? "m'" . v:count : '') . 'k' : 'gk'

onoremap <expr> j v:count ? (v:count > 5 ? "m'" . v:count : '') . 'j' : 'gj'
onoremap <expr> k v:count ? (v:count > 5 ? "m'" . v:count : '') . 'k' : 'gk'

nnoremap L $
nnoremap H ^
xnoremap L $
xnoremap H ^
onoremap L $
onoremap H ^

nnoremap <C-j> H
nnoremap <C-h> M
nnoremap <C-k> L

xnoremap <C-j> H
xnoremap <C-h> M
xnoremap <C-k> L

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

" Instert Mode
inoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')
inoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1), '\%' . virtcol('.') . 'v\%(\k\+\\|.\)')


imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : "<S-Tab>"
imap <expr> <Tab>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : "<Tab>"

inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })
inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })
inoremap <silent><expr> <C-e>     compe#close('<C-e>')
inoremap <silent><expr> <C-Space> compe#complete()

" Stop The Deselecting
vnoremap < <gv
vnoremap > >gv

" Terminal
tnoremap <Esc> <C-\><C-n>

" Panel Specific Mappings
augroup panelMappings
  au filetype Outline map <buffer> o <cmd>lua require('symbols-outline')._goto_location(true)<cr><cmd>sleep 2<cr><cmd>SymbolsOutlineClose<cr>
  au filetype qf map <buffer> <esc> <cmd>q<cr>
  au filetype help map <buffer> <esc> <cmd>q<cr>
  au filetype vim-plug map <buffer> <esc> <cmd>q<cr>
  au filetype juliadoc map <buffer> <esc> <cmd>q<cr>
augroup END

" Vim Hop
map s <cmd>lua require'hop'.hint_char1()<cr>
map S <cmd>lua require'hop'.hint_lines()<cr>
omap s <cmd> lua require'hop'.hint_char1()<cr>
omap S <cmd> lua require'hop'.hint_lines()<cr>


" Word Motion Command
let g:wordmotion_prefix = '$'

" Text Object Mappings
xnoremap <silent> Ac :<C-u>call TextObjWordBasedColumn("aW")<cr>
xnoremap <silent> Ic :<C-u>call TextObjWordBasedColumn("iW")<cr>
onoremap <silent> Ac :call TextObjWordBasedColumn("aW")<cr>
onoremap <silent> Ic :call TextObjWordBasedColumn("iW")<cr>

onoremap <silent>Ai :<C-u>cal <Sid>HandleTextObjectMapping(0, 1, 0, [line("."), line("."), col("."), col(".")])<CR>
onoremap <silent>Ii :<C-u>cal <Sid>HandleTextObjectMapping(1, 1, 0, [line("."), line("."), col("."), col(".")])<CR>
vnoremap <silent>Ai :<C-u>cal <Sid>HandleTextObjectMapping(0, 1, 1, [line("'<"), line("'>"), col("'<"), col("'>")])<CR><Esc>gv
vnoremap <silent>Ii :<C-u>cal <Sid>HandleTextObjectMapping(1, 1, 1, [line("'<"), line("'>"), col("'<"), col("'>")])<CR><Esc>gv

" Nvim Comment
lua require('nvim_comment').setup({comment_empty = false})
nmap <c-_> gcc
xmap <c-_> gc

" Arrows
nmap <silent> <right>   <cmd>BufferLineCycleNext<cr>
nmap <silent> <left>    <cmd>BufferLineCyclePrev<cr>
nmap <silent> <S-right> <nop>
nmap <silent> <S-left>  <nop>
nmap <silent> <C-right> <nop>
nmap <silent> <C-left>  <nop>
nmap <silent> <C-up>    <cmd>try <bar> cnext     <bar> catch /E553/ <bar> cfirst <bar> endtry<CR>
nmap <silent> <C-down>  <cmd>try <bar> cprevious <bar> catch /E553/ <bar> clast  <bar> endtry<CR>
nmap <silent> <S-up>    <cmd>try <bar> lnext     <bar> catch /E553/ <bar> lfirst <bar> endtry<CR>
nmap <silent> <S-down>  <cmd>try <bar> lprevious <bar> catch /E553/ <bar> llast  <bar> endtry<CR>

" Unmap Pluggins
let g:splitjoin_split_mapping = ''
let g:splitjoin_join_mapping = ''
let g:UnconditionalPaste_no_mappings = 1
let g:caser_no_mappings	= 1
let g:doge_enable_mappings=0

" Expression Mapping (to go in which-key)
nmap <expr> <plug>(Telescope-grep)  "<cmd>Telescope grep_string search=" . input("Grep For > ") . "<CR>"
nmap <expr> <plug>(Telescope-Vimgrep-files)  "<cmd>noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj " . input("In what files? > ") . "<cr><cmd>Telescope quickfix<cr>"
nmap <expr> <plug>(Telescope-Vimgrep-all)  "<cmd>noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj **/* <cr><cmd>Telescope quickfix<cr>"
nmap <expr> <plug>(Telescope-locate)  "<cmd>Clocate! " . input("What would you like to locate? ")  . "<cr><cmd>Telescope quickfix<cr>"
nmap <expr> <plug>(Telescope-find)  "<cmd>Cfind! " . input("What would you like to find? ")  . "<cr><cmd>Telescope quickfix<cr>"
nmap <expr> <plug>(Telescope-relevant)  "<cmd>Telescope fd default_text=" . split(expand("%:t:r"), '_')[0] . " prompt_prefix= <cr>"
nmap <expr> <plug>(Quickfix-find)  "<cmd>Cfind! " . input("What would you like to find? ")  . "<cr><cmd>Trouble quickfix<cr>"
nmap <expr> <plug>(Quickfix-locate)  "<cmd>Clocate! " . input("What would you like to locate? ")  . "<cr><cmd>Trouble quickfix<cr>"
nmap <expr> <plug>(Quickfix-vimgrep-files)  "<cmd>noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj **/* <cr><cmd>Trouble quickfix<cr>"
nmap <expr> <plug>(Quickfix-vimgrep-all)  "<cmd>noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj " . input("In what files? > ") . "<cr><cmd>Trouble quickfix<cr>"
nmap <expr> <plug>(Loclist-find)  "<cmd>Lfind! " . input("What would you like to find? ")  . "<cr><cmd>Trouble loclist<cr>"
nmap <expr> <plug>(Loclist-locate)  "<cmd>Llocate! " . input("What would you like to locate? ")  . "<cr><cmd>Trouble loclist<cr>"
nmap <expr> <plug>(Loclist-vimgrep-files)  "<cmd>noautocmd lvimgrep /" . input("What would you like to vimgrep? > ") . "/gj **/* <cr><cmd>Trouble loclist<cr>"
nmap <expr> <plug>(Loclist-vimgrep-all)  "<cmd>noautocmd lvimgrep /" . input("What would you like to vimgrep? > ") . "/gj " . input("In what files? > ") . "<cr><cmd>Trouble loclist<cr>"
nmap <expr> <plug>(Julia-precompile)   "<cmd>TermExec cmd='clear'<cr><cmd>TermExec cmd=\"julia --project -e 'using Pkg; Pkg.precompile()'\"<cr>"
nmap <expr> <plug>(Julia-test)         "<cmd>TermExec cmd='clear'<cr><cmd>TermExec cmd=\"julia --project -e 'using Pkg; Pkg.test()'\"<cr>"

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
call  PMRegisterPanelView('bottom', 'term',             'ToggleTerm',                           'ToggleTerm')
call  PMRegisterPanelView('bottom', '2term',            '2ToggleTerm',                          'windo if &filetype == "toggleterm" | q | endif')
call  PMRegisterPanelView('bottom', '3term',            '3ToggleTerm',                          'windo if &filetype == "toggleterm" | q | endif')
call  PMRegisterPanelView('bottom', '4term',            '4ToggleTerm',                          'windo if &filetype == "toggleterm" | q | endif')
call  PMRegisterPanelView('bottom', '5term',            '5ToggleTerm',                          'windo if &filetype == "toggleterm" | q | endif')
call  PMRegisterPanelView('right',  'symbols',          'SymbolsOutlineOpen',                   'SymbolsOutlineClose')
call  PMRegisterPanelView('top',    'gitdiff',          'DiffviewOpen',                         'DiffviewClose')

function CloseAllPanels()
  UndotreeHide
  windo if &buftype == "quickfix" || &buftype == "locationlist" | q | endif
  windo if &filetype == "toggleterm" | q | endif
  NvimTreeClose
  TroubleClose
  TroubleClose 
  SymbolsOutlineClose
  DiffviewClose
  windo if &filetype == "help" | q | endif
  windo if &filetype == "juliadoc" | q | endif
endfunction


lua << EOF
require('config_main')
EOF

silent! !git rev-parse --is-inside-work-tree
if v:shell_error == 0
  lua require("which-key").register({["<leader>f"]={f = {"<cmd>Telescope git_files<cr>", "Find Files"}}})
else
  lua require("which-key").register({["<leader>f"]={f = {"<cmd>Telescope fd<cr>", "Find Files"}}})
endif

" TODO: Move to Latex File
let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
let g:vimtex_view_automatic=1
let g:vimtex_view_forward_search_on_start=1
nnoremap <localleader>fb <cmd>Telescope bibtex bibtex<cr>

autocmd FileType markdown let b:compe_latex_insert_code = v:true
autocmd FileType tex let b:compe_latex_insert_code = v:true
let g:vimtex_compiler_latexmk = {
    \ 'build_dir' : '',
    \ 'callback' : 1,
    \ 'continuous' : 0,
    \ 'executable' : 'latexmk',
    \ 'hooks' : [],
    \ 'options' : [
    \   '-verbose',
    \   '-file-line-error',
    \   '-synctex=1',
    \   '-interaction=nonstopmode',
    \ ],
    \}

autocmd BufNewFile,BufRead *.jl set filetype=julia
autocmd FileType julia set commentstring=#%s

redraw

