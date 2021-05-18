let $SHELL = "/bin/zsh"
set shell=/bin/zsh

call plug#begin('~/.config/nvim/pluged')
  Plug 'lambdalisue/suda.vim'
  Plug '907th/vim-auto-save'
  Plug 'farmergreg/vim-lastplace'
  Plug 'nvim-lua/plenary.nvim'

  " Project Managment
  Plug 'tpope/vim-obsession'
  Plug 'tpope/vim-projectionist'

  " Git
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-rhubarb'
  Plug 'lewis6991/gitsigns.nvim', {'branch': 'sec'}
  Plug 'junegunn/gv.vim'
  Plug 'drzel/vim-repo-edit'
  Plug 'rhysd/committia.vim'
  Plug 'sindrets/diffview.nvim'

  " Registers
  " Plug 'svermeulen/vim-cutlass'
  Plug 'svermeulen/vim-subversive'
  " Plug 'junegunn/vim-peekaboo'

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

  " UI Stuff
  " Plug 'junegunn/goyo.vim'
  " Plug 'junegunn/limelight.vim'
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
  " Plug 'inkarkat/vim-SpellCheck'

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
  " Plug 'GoldsteinE/compe-latex-symbols'

  " Telescope
  Plug 'nvim-lua/popup.nvim'
  Plug 'nvim-telescope/telescope.nvim'
  Plug 'nvim-telescope/telescope-symbols.nvim'
  Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make'}
  Plug 'nvim-telescope/telescope-media-files.nvim'
  Plug 'tkmpypy/telescope-jumps.nvim'
  Plug 'nvim-telescope/telescope-bibtex.nvim'

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
nmap <silent> <right>   <cmd>try <bar> cnext     <bar> catch /E553/ <bar> cfirst <bar> endtry<CR>
nmap <silent> <left>    <cmd>try <bar> cprevious <bar> catch /E553/ <bar> clast  <bar> endtry<CR>
nmap <silent> <C-right> <cmd>try <bar> lnext     <bar> catch /E553/ <bar> lfirst <bar> endtry<CR>
nmap <silent> <C-left>  <cmd>try <bar> lprevious <bar> catch /E553/ <bar> llast  <bar> endtry<CR>
nmap <silent> <S-right> <cmd>BufferLineCycleNext<cr>
nmap <silent> <S-left>  <cmd>BufferLineCyclePrev<cr>
nmap <silent> <C-up>    <nop>
nmap <silent> <C-down>  <nop>
nmap <silent> <S-up>    <nop>
nmap <silent> <S-down>  <nop>

" Unmap Pluggins
let g:splitjoin_split_mapping = ''
let g:splitjoin_join_mapping = ''
let g:UnconditionalPaste_no_mappings = 1

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
call  PMRegisterPanelView('bottom', 'loclistFilter',    'lwindow',                              'windo if &buftype == "quickfix" || &buftype == "locationlist" | q | endif')
call  PMRegisterPanelView('bottom', 'quickfixFilter',   'cwindow',                              'windo if &buftype == "quickfix" || &buftype == "locationlist" | q | endif')
call  PMRegisterPanelView('bottom', 'term',             'ToggleTerm',                           'ToggleTerm')
call  PMRegisterPanelView('bottom', '2term',            '2ToggleTerm',                          'windo if &filetype == "toggleterm" | q | endif')
call  PMRegisterPanelView('bottom', '3term',            '3ToggleTerm',                          'windo if &filetype == "toggleterm" | q | endif')
call  PMRegisterPanelView('bottom', '4term',            '4ToggleTerm',                          'windo if &filetype == "toggleterm" | q | endif')
call  PMRegisterPanelView('bottom', '5term',            '5ToggleTerm',                          'windo if &filetype == "toggleterm" | q | endif')
call  PMRegisterPanelView('right',  'symbols',          'SymbolsOutlineOpen',                   'SymbolsOutlineClose')
call  PMRegisterPanelView('top',    'gitdiff',          'call DiffviewOpenAndSet()',            'DiffviewClose')

function DiffviewOpenAndSet()
  DiffviewOpen
  set filetype=DiffviewFiles
endfunction

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

-- Zen Mode

require("zen-mode").setup {
  window = {
    backdrop = 0.95, -- shade the backdrop of the Zen window. Set to 1 to keep the same as Normal
    -- height and width can be:
    -- * an absolute number of cells when > 1
    -- * a percentage of the width / height of the editor when <= 1
    width = 120, -- width of the Zen window
    height = 1, -- height of the Zen window
    -- by default, no options are changed for the Zen window
    -- uncomment any of the options below, or add other vim.wo options you want to apply
    options = {
      signcolumn = "no",
      number = false, -- disable number column
      relativenumber = false, -- disable relative numbers
      scrolloff = 999,
      -- cursorline = false, -- disable cursorline
      -- cursorcolumn = false, -- disable cursor column
      -- foldcolumn = "0", -- disable fold column
      -- list = false, -- disable whitespace characters
    },
  },
  plugins = {
    gitsigns = true, -- disables git signs
    tmux = true, -- disables the tmux statusline
    -- this will change the font size on kitty when in Zen mode
    -- to make this work, you need to set the following kitty options:
    -- - allow_remote_control socket-only
    -- - listen_on unix:/tmp/kitty
    kitty = {
      enabled = false,
      font = "+1", -- font size increment
    },
  },
  -- callback where you can add custom code when the Zen window opens
  on_open = function(win)
  end,
  -- callback where you can add custom code when the Zen window closes
  on_close = function()
  end,
}

-- BufferLine

require'bufferline'.setup{
 options = {
    view = "multiwindow",
    numbers = "none", 
    number_style = "none",
    mappings = false,
    buffer_close_icon= '',
    modified_icon = '●',
    close_icon = '',
    left_trunc_marker = '',
    right_trunc_marker = '',
    max_name_length = 18,
    max_prefix_length = 15, 
    tab_size = 18,
    diagnostics = "nvim_lsp",
    diagnostics_indicator = function(count, level, diagnostics_dict)
      local icon = level:match("error") and "" or (level:match("warning") and "" or "")
      return " " .. icon .. count
    end,
    show_buffer_close_icons = true, 
    show_close_icon = true, 
    show_tab_indicators = true,
    persist_buffer_sort = true,
    separator_style = "slant", 
    enforce_regular_tabs = true, 
    always_show_bufferline = true,
  }
}

-- LuaLine Status Line

require('lualine').setup{
  options = {
    theme = 'tokyonight',
    -- theme = 'material-nvim',
    section_separators = {'', ''},
    component_separators = {'', ''},
    extensions = { 'nvim-tree', 'fugitive' },
  },
  sections = {
    lualine_a = {{'mode'}},
    lualine_b = {{'branch'}, { 'filename', file_status = true }},
    lualine_c = {
      { 'diagnostics', 
        sources={ 'nvim_lsp' },
        sections={'error', 'warn', 'info'},
        color_error='#f85e84',
        color_warn='#e5c463',
        color_info='#7accd7',
        {error = '', warn = '', info = ''},
        'lsp-progress'
      }, 
    },
    lualine_x = {
      { 'diff', 
        color_added='#9ecd6f',
        color_modified='#7accd7',
        color_removed='#f85e84',
        symbols={added = '+', modified = '~', removed = '-'}
      }, 
      'fileformat', 'filetype'
    },
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {'filename'},
    lualine_x = {'progress'},
    lualine_y = {},
    lualine_z = {}
  }
}

-- Nvim Tree
vim.g.nvim_tree_disable_netrw = 0
vim.g.nvim_tree_hijack_netrw = 0
vim.g.nvim_tree_git_hl = 1
vim.g.nvim_tree_lsp_diagnostics = 1
vim.g.netrw_liststyle = 3
vim.g.netrw_preview=1
vim.g.nvim_tree_width = 40 

vim.g.nvim_tree_icons = {
    default = '',
    symlink = '',
    git = {unstaged = "", staged = "✓", unmerged = "", renamed = "➜", untracked = ""},
    folder = {default = "", open = "", empty = "", empty_open = "", symlink = ""}
}
local tree_cb = require'nvim-tree.config'.nvim_tree_callback
vim.g.nvim_tree_bindings = {
  ["<CR>"]           = tree_cb("edit"),
  ["o"]              = "<cmd>lua require('nvim-tree').on_keypress('edit')<cr><cmd>sleep 250m<cr><cmd>NvimTreeClose<cr>",
  ["<2-LeftMouse>"]  = tree_cb("edit"),
  ["<2-RightMouse>"] = tree_cb("cd"),
  ["<C-]>"]          = tree_cb("cd"),
  ["<C-v>"]          = tree_cb("vsplit"),
  ["<C-x>"]          = tree_cb("split"),
  ["<C-t>"]          = tree_cb("tabnew"),
  ["<"]              = tree_cb("prev_sibling"),
  [">"]              = tree_cb("next_sibling"),
  ["<BS>"]           = tree_cb("close_node"),
  ["<S-CR>"]         = tree_cb("close_node"),
  ["<Tab>"]          = tree_cb("preview"),
  ["I"]              = tree_cb("toggle_ignored"),
  ["H"]              = tree_cb("toggle_dotfiles"),
  ["R"]              = tree_cb("refresh"),
  ["a"]              = tree_cb("create"),
  ["dd"]              = tree_cb("remove"),
  ["r"]              = tree_cb("rename"),
  ["<C-r>"]          = tree_cb("full_rename"),
  ["x"]              = tree_cb("cut"),
  ["cc"]              = tree_cb("copy"),
  ["p"]              = tree_cb("paste"),
  ["[c"]             = tree_cb("prev_git_item"),
  ["]c"]             = tree_cb("next_git_item"),
  ["-"]              = tree_cb("dir_up"),
  ["q"]              = tree_cb("close"),
  ["<esc>"]              = tree_cb("close"),
}

-- DiffView.nvim

local cb = require'diffview.config'.diffview_callback
require'diffview'.setup {
  diff_binaries = false,    -- Show diffs for binaries
  file_panel = {
    width = 35,
    use_icons = true        -- Requires nvim-web-devicons
  },
  key_bindings = {
    -- The `view` bindings are active in the diff buffers, only when the current
    -- tabpage is a Diffview.
    view = {
      ["<tab>"]     = cb("select_next_entry"),  -- Open the diff for the next file 
      ["<s-tab>"]   = cb("select_prev_entry"),  -- Open the diff for the previous file
      ["<leader>t"] = cb("focus_files"),        -- Bring focus to the files panel
      -- ["<leader>b"] = cb("toggle_files"),       -- Toggle the files panel.
    },
    file_panel = {
      ["j"]         = cb("next_entry"),         -- Bring the cursor to the next file entry
      ["<down>"]    = cb("next_entry"),
      ["k"]         = cb("prev_entry"),         -- Bring the cursor to the previous file entry.
      ["<up>"]      = cb("prev_entry"),
      ["<cr>"]      = cb("select_entry"),       -- Open the diff for the selected entry.
      ["o"]         = "<cmd>lua require('diffview').on_keypress('select_entry')<cr><cmd>sleep 100m<cr><cmd>DiffviewToggleFiles<cr>",
      ["p"]         = "<cmd>lua require('diffview').on_keypress('select_entry')<cr><cmd>DiffviewFocusFiles<cr>",
      ["R"]         = cb("refresh_files"),      -- Update stats and entries in the file list.
      ["<tab>"]     = cb("select_next_entry"),
      ["<s-tab>"]   = cb("select_prev_entry"),
      ["<leader>t"] = cb("focus_files"),
      ["<leader>b"] = cb("toggle_files"),
    }
  }
}

-- Lsp Trouble

require("trouble").setup {
  height = 20, 
  icons = true, 
  mode = "workspace", 
  fold_open = "", 
  fold_closed = "", 
  action_keys = { 
      cancel = "q", -- cancel the preview and get back to your last window / buffer / cursor
      close = "<esc>", -- close the list
      refresh = "r", -- manually refresh
      jump = "<cr>", -- jump to the diagnostic or open / close folds
      jump_close = {"o"}, -- jump to the diagnostic and close the list
      toggle_mode = "m", -- toggle between "workspace" and "document" mode
      preview = "P", -- preview the diagnostic location
      toggle_preview = "p", -- preview the diagnostic location
      close_folds = {"zM", "zm"}, -- close all folds
      open_folds = {"zR", "zr"}, -- open all folds
      toggle_fold = {"zA", "za"}, -- toggle fold of current file
      previous = "k", -- preview item
      next = "j" -- next item
  },
  indent_lines = true, -- add an indent guide below the fold icons
  auto_open = false, 
  auto_close = false, 
  auto_preview = false, -- automatically preview the location of the diagnostic. <esc> to close preview and go back
  signs = {
      -- icons / text used for a diagnostic
      error       = "",
      warning     = "",
      hint        = "",
      information = "",
      other = "﫠",
  },
  use_lsp_diagnstic_signs = false -- enabling this will use the s
}

-- Symbols Outline

vim.g.symbols_outline = {
    highlight_hovered_item = true,
    show_guides = true,
    position = 'right',
    keymaps = {
        close = "<Esc>",
        goto_location = "<cr>",
        -- focus_location = "<cr>",
        hover_symbol = "<C-space>",
        rename_symbol = "r",
        code_actions = "a",
    },
    lsp_blacklist = {},
}

-- Toggle Terminal

require"toggleterm".setup{
  size = 25,
  open_mapping = [[<c-\>]],
  hide_numbers = true, -- hide the number column in toggleterm buffers
  shade_filetypes = {},
  shade_terminals = false,
  shading_factor = '1', -- the degree by which to darken to terminal colour, default: 1 for dark backgrounds, 3 for light
  start_in_insert = false,
  persist_size = true,
  direction = 'horizontal',
  shell = 'fish',
}

-- Treesitter

require'nvim-treesitter.configs'.setup {
  highlight = { enable = true, },
  indent = { enable = true },
  rainbow = { enable = true },
  context = { enable = true },
  context_commentstring = { enable = true },
  matchup = {
    enable = true,
  },
  textobjects = {
    select = {
      enable = true,
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["ao"] = "@block.outer",
        ["io"] = "@block.inner",
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
      },
    },
    swap = {
      enable = true,
      swap_next = {
        -- ["g>"] = "@parameter.outer",
      },
      swap_previous = {
        -- ["g<"] = "@parameter.outer",
      },
    },
    move = {
      enable = true,
      goto_next_start = {
        ["]o"] = "@block.outer",
        ["]f"] = "@function.outer",
      },
      goto_next_end = {
        ["]O"] = "@block.outer",
        ["]F"] = "@function.outer"
      },
      goto_previous_start = {
        ["[o"] = "@block.outer",
        ["[f"] = "@function.outer",
      },
      goto_previous_end = {
        ["[O"] = "@block.outer",
        ["[F"] = "@function.outer",
      },
    },
    lsp_interop = {
       enable = true,
       peek_definition_code = {
         ["<leader>pD"] = "@function.outer",
         ["<leader>pD"] = "@class.outer",
       },
     },
  },
  refactor = {
    smart_rename = {
      enable = true,
      keymaps = {
        smart_rename = "<leader>rt",
        -- list_definitions_toc = "<leader>od",
      },
    },
    navigation = {
      enable = true,
      keymaps = {
        -- goto_definition = "<leader>od",
        -- list_definitions = "<leader>fs",
        -- goto_next_usage = "<leader>or",
      },
    },
  },
}
require('nvim-biscuits').setup({
  default_config = {
    min_distance = 10,
  },
  language_config = {
    haskell = {
      disabled = true
    }
  }
})

-- Compleation Setup

vim.o.completeopt = "menuone,noselect"
require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = true;

  source = {
    path = true;
    buffer = true;
    calc = true;
    nvim_lsp = true;
    nvim_lua = true;
    nvim_treesitter = false;
    -- latex_symbols = true;
    vsnip = {
      priority = 1000,
    },
    omni = {
        filetypes = tex
    }
  };
}

-- Compe Use Tab Keys
local t = function(str)
  return vim.api.nvim_replace_termcodes(str, true, true, true)
end

local check_back_space = function()
    local col = vim.fn.col('.') - 1
    if col == 0 or vim.fn.getline('.'):sub(col, col):match('%s') then
        return true
    else
        return false
    end
end

-- Use (s-)tab to:
--- move to prev/next item in completion menuone
--- jump to prev/next snippet's placeholder
_G.tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-n>"
  elseif vim.fn.call("vsnip#available", {1}) == 1 then
    return t "<Plug>(vsnip-expand-or-jump)"
  elseif check_back_space() then
    return t "<Tab>"
  else
    return vim.fn['compe#complete']()
  end
end
_G.s_tab_complete = function()
  if vim.fn.pumvisible() == 1 then
    return t "<C-p>"
  elseif vim.fn.call("vsnip#jumpable", {-1}) == 1 then
    return t "<Plug>(vsnip-jump-prev)"
  else
    return t "<S-Tab>"
  end
end

vim.api.nvim_set_keymap("i", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<Tab>", "v:lua.tab_complete()", {expr = true})
vim.api.nvim_set_keymap("i", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})
vim.api.nvim_set_keymap("s", "<S-Tab>", "v:lua.s_tab_complete()", {expr = true})

-- Snippets Settup

local remap = vim.api.nvim_set_keymap
local npairs = require('nvim-autopairs')
_G.MUtils= {}

vim.g.completion_confirm_key = ""
MUtils.completion_confirm=function()
  if vim.fn.pumvisible() ~= 0  then
    if vim.fn.complete_info()["selected"] ~= -1 then
      return vim.fn["compe#confirm"](npairs.esc("<cr>"))
    else
      return npairs.esc("<cr>")
    end
  else
    return npairs.autopairs_cr()
  end
end

remap('i' , '<CR>','v:lua.MUtils.completion_confirm()', {expr = true , noremap = true})
npairs.setup({
    check_ts = true,
})

require('nvim-treesitter.configs').setup {
    autopairs = {enable = true}
}

local ts_conds = require('nvim-autopairs.ts-conds')

-- Lsp Settup

local nvim_lsp = require('lspconfig')
local saga = require 'lspsaga'

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

vim.fn.sign_define( "LspDiagnosticsSignError", {texthl = "LspDiagnosticsSignError", text = "", numhl = "LspDiagnosticsSignError"})
vim.fn.sign_define( "LspDiagnosticsSignWarning", {texthl = "LspDiagnosticsSignWarning", text = "", numhl = "LspDiagnosticsSignWarning"})
vim.fn.sign_define( "LspDiagnosticsSignHint", {texthl = "LspDiagnosticsSignHint", text = "", numhl = "LspDiagnosticsSignHint"})
vim.fn.sign_define( "LspDiagnosticsSignInformation", {texthl = "LspDiagnosticsSignInformation", text = "", numhl = "LspDiagnosticsSignInformation"})

local custom_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

	print("LSP started.");
  capabilities = capabilities
  -- require'virtualtypes'.on_attach()
  require'lsp_signature'.on_attach()
  require 'illuminate'.on_attach(client)
  saga.init_lsp_saga {
    use_saga_diagnostic_sign = true,
    error_sign = '',
    warn_sign = '',
    hint_sign = '',
    infor_sign = '',
    dianostic_header_icon = ' ',
    code_action_icon = '',
    code_action_prompt = {
      enable = true,
      sign = false,
      sign_priority = 20,
      virtual_text = true,
    },
    finder_definition_icon = '  ',
    finder_reference_icon = '  ',
    max_preview_lines = 10, 
    finder_action_keys = { open = '<cr>', vsplit = 's',split = 'i',quit = '<esc>',scroll_down = '<C-f>', scroll_up = '<C-b>'},
    code_action_keys = { quit = '<esc>',exec = '<CR>' },
    rename_action_keys = { quit = '<esc>',exec = '<CR>' },
    definition_preview_icon = '  ',
    border_style = "single",
    rename_prompt_prefix = '➤',
  }

  local opts = { noremap=true, silent=true }
  require("which-key").register({
    ["<leader>"] = {
      ["."] = {"<cmd>Lspsaga code_action<CR>", "Code Actions"},
      ["="] = {"<cmd>lua vim.lsp.buf.formatting()<CR>", "Format"},
      o = {
        d = {"<cmd>Telescope lsp_definitions<cr>", "Definitions"},
        r = {"<cmd>Telescope lsp_references<cr>", "References"},
        i = {"<cmd>lua vim.lsp.buf.implementation()<CR>", "implementations"},
      },
      f = {
        s = {"<cmd>Telescope lsp_workspace_symbols<cr>", "Symbols"},
        S = {"<cmd>Telescope lsp_document_symbols<cr>", "Symbols (buffer)"},
      },
      p = {
        name = "preview",
        p = {"<Cmd>Lspsaga hover_doc<CR>", "Documentation"},
        s = {"<cmd>Lspsaga signature_help<CR>", "Signiture"},
        d = {"<cmd>Lspsaga preview_definition<CR>", "Definition"},
        e = {"<cmd>Lspsaga show_line_diagnostics<CR>", "Diagnostics"},
        a = {"<cmd>Lspsaga lsp_finder<CR>", "All"},
        l = {"<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", "Workspace Directory"},
      },
      r = {
        r = {"<cmd>Lspsaga rename<CR>", "Rename (LSP)"},
      },
    },
    ["["] = {
      e = {"<cmd>Lspsaga diagnostic_jump_prev<CR>", "Error"},
    },
    ["]"] = {
      e = {"<cmd>Lspsaga diagnostic_jump_next<CR>", "Error"},
    },
  })
  require("which-key").register({
    ["<leader>"] = {
      ["."] = {"<cmd>Lspsaga range_code_action<CR>", "Code Actions"},
      ["="] = {"<cmd>lua vim.lsp.buf.formatting()<CR>", "Format"},
    }
  }, {mode="v"})

  vim.api.nvim_exec([[
    autocmd CursorHold * :Lspsaga show_cursor_diagnostics
  ]], false)

  -- Set autocommands conditional on server_capabilities
  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_exec([[
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]], false)
  end
end
require'lspconfig'.julials.setup{
    on_attach=custom_attach,
    root_dir = nvim_lsp.util.root_pattern('Project.toml', 'git', vim.fn.getcwd());
}

require'lsp_signature'.on_attach({
  bind = true, -- This is mandatory, otherwise border config won't get registered.
               -- If you want to hook lspsaga or other signature handler, pls set to false
  doc_lines = 10, -- only show one line of comment set to 0 if you do not want API comments be shown

  hint_enable = true, -- virtual hint enable
  hint_prefix = "🐼 ",  -- Panda for parameter
  hint_scheme = "String",

  handler_opts = {
    border = "single"   -- double, single, shadow, none
  },
  decorator = {"`", "`"}  
})
require'lspinstall'.setup()
local servers = require'lspinstall'.installed_servers()
  if server == 'texlab' then
    require'lspconfig'texlab.setup{
        on_attach=custom_attach,
        settings = {
            bibtex = {
              formatting = {
                lineLength = 120
              }
            },
            latex = {
              build = {
                args = { "-pdf", "-interaction=nonstopmode", "-synctex=1", "%f" },
                executable = "latexmk",
                onSave = true,
                forwardSearchAfter = true
              },
              forwardSearch = {
                executable = "zathura",
                args = {"--synctex-forward", "%l:1:%f", "%p"},
                onSave = false
              },
              lint = {
                onChange = true
              }
            }
        }
    }
  else
    for _, server in pairs(servers) do
      require'lspconfig'[server].setup{on_attach=custom_attach}
    end
  end

require'lspconfig'.hls.setup{
  on_attach=custom_attach,
  cmd = { "haskell-language-server-wrapper", "--lsp" },
  filetypes = { "haskell", "lhaskell" },
  root_dir = nvim_lsp.util.root_pattern("*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml", ".git"),
  lspinfo = function (cfg)
        -- return "specific"
        if cfg.settings.languageServerHaskell.logFile or false then
          return "logfile: "..cfg.settings.languageServerHaskell.logFile
        end
        return ""
      end;
}

local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'

require('lspkind').init({
     with_text = true,
     symbol_map = {
       Text = '',
       Method = 'ƒ',
       Function = '',
       Constructor = '',
       Variable = '',
       Class = '',
       Interface = 'ﰮ',
       Module = '',
       Property = '',
       Unit = '',
       Value = '',
       Enum = '了',
       Keyword = '',
       Snippet = '﬌',
       Color = '',
       File = '',
       Folder = '',
       EnumMember = '',
       Constant = '',
       Struct = ''
     },
})

vim.g.diagnostic_auto_popup_while_jump = 0
vim.g.diagnostic_enable_virtual_text = 0
vim.g.diagnostic_enable_underline = 0
vim.g.completion_timer_cycle = 200 

-- Telescope Settup

local actions = require('telescope.actions')
require"telescope".load_extension("bibtex")
require"telescope".load_extension("media_files")
local trouble = require("trouble.providers.telescope")

require('telescope').setup{
  defaults = {
    vimgrep_arguments = {
      'rg',
      '--color=never',
      '--no-heading',
      '--with-filename',
      '--line-number',
      '--column',
      '--smart-case'
    },
    prompt_position = "bottom",
    prompt_prefix = "> ",
    selection_caret = "> ",
    entry_prefix = "  ",
    initial_mode = "insert",
    selection_strategy = "reset",
    sorting_strategy = "descending",
    layout_strategy = "flex",
    layout_defaults = {
      horizontal = {
        mirror = false,
      },
      vertical = {
        mirror = false,
      },
    },
    file_sorter =  require'telescope.sorters'.get_fuzzy_file,
    file_ignore_patterns = {},
    generic_sorter =  require'telescope.sorters'.get_generic_fuzzy_sorter,
    shorten_path = true,
    winblend = 0,
    width = 0.75,
    preview_cutoff = 1,
    results_height = 1,
    results_width = 0.8,
    border = {},
    borderchars = { '─', '│', '─', '│', '╭', '╮', '╯', '╰' },
    color_devicons = true,
    use_less = true,
    set_env = { ['COLORTERM'] = 'truecolor' }, -- default = nil,
    file_previewer = require'telescope.previewers'.vim_buffer_cat.new,
    grep_previewer = require'telescope.previewers'.vim_buffer_vimgrep.new,
    qflist_previewer = require'telescope.previewers'.vim_buffer_qflist.new,

    mappings = {
      i = {
        ["<C-q>"] = actions.smart_send_to_qflist,
        ["<C-space>"] = actions.toggle_selection + actions.move_selection_worse,
        ["<C-a>"] = actions.smart_add_to_qflist,
        ["<C-j>"] = actions.move_to_top,
        ["<C-h>"] = actions.move_to_middle,
        ["<C-k>"] = actions.move_to_bottom,
        ["<c-e>"] = trouble.open_with_trouble 
      },
      n = {
        ["<c-e>"] = trouble.open_with_trouble 
      },
    },
  },
  extensions = {
    bibtex = {
          depth = 2,
          global_files = { '/home/oleete/UniDrive/1_Thesis/0.1_LaTeX/Citations.bib'},
    },
    fzf = {
          override_generic_sorter = false, -- override the generic sorter
          override_file_sorter = true,     -- override the file sorter
          case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
    },
    media_files = {
    },
  },
}
require('telescope').load_extension('fzf')
require('telescope').load_extension('bibtex')

-- Gitsigns Settup

require('gitsigns').setup {
  signs = {
    add          = {hl = 'GitSignsAdd',    text = '▌', numhl='GitSignsAddNr'   , linehl='GitSignsAddLn'},
    change       = {hl = 'GitSignsChange', text = '▌', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
    delete       = {hl = 'GitSignsDelete', text = '_', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    topdelete    = {hl = 'GitSignsDelete', text = '‾', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    changedelete = {hl = 'GitSignsDelete', text = '~', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
  },
  signs_sec = {
    add          = {hl = 'Normal', text = '▌'},
    change       = {hl = 'Normal', text = '▌'},
    delete       = {hl = 'Normal', text = '_'},
    topdelete    = {hl = 'Normal', text = '‾'},
    changedelete = {hl = 'Normal', text = '~'},
    base = 'HEAD'
  },
  numhl = true,
  linehl = false,
  keymaps = {
    -- Default keymap options
    noremap = true,
    buffer = true,

    ['n ]h'] = { expr = true, "&diff ? ']c' : '<cmd>lua require\"gitsigns\".next_hunk()<CR>'"},
    ['n [h'] = { expr = true, "&diff ? '[c' : '<cmd>lua require\"gitsigns\".prev_hunk()<CR>'"},

    -- Text objects
    ['o ih'] = ':<C-U>lua require"gitsigns".select_hunk()<CR>',
    ['x ih'] = ':<C-U>lua require"gitsigns".select_hunk()<CR>'
  },
  watch_index = {
    interval = 1000
  },
  current_line_blame = false,
  sign_priority = 6,
  update_debounce = 100,
  status_formatter = nil, -- Use default
  use_decoration_api = true,
  use_internal_diff = true,  -- If luajit is present
}

-- BQF Settup

require('bqf').setup({
    auto_enable = true,
    preview = {
        win_height = 12,
        win_vheight = 12,
        delay_syntax = 80,
        border_chars = {'│', '│', '─', '─', '╭', '╮', '╰', '╯', '█'}
    },
    func_map = {
        stoggledown = '<c-space>',
        stogglevm = '<c-space>',
        tab = '<c-t>',
        split = '<c-x>',
        vsplit = '<c-v>',
        prevfile = 'k',
        nextfile = 'j',
        vsplit = '<c-v>',
    },
})

require("todo-comments").setup {
  signs = true, -- show icons in the signs column
  -- keywords recognized as todo comments
  keywords = {
    FIX = { icon = " ", color = "error", alt = { "FIXME", "BUG", "FIXIT", "FIX", "ISSUE" }, },
    TODO = { icon = " ", color = "info" },
    HACK = { icon = " ", color = "warning", alt = {"JANK", "WORKAROUND"}},
    WARN = { icon = " ", color = "warning", alt = { "WARNING"} },
    PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
    NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
  },
  highlight = {
    before = "fg",
    keyword = "wide",
    after = "fg",
  },
  -- list of named colors where we try to extract the guifg from the
  -- list of hilight groups or use the hex color if hl not found as a fallback
  colors = {
    error = { "LspDiagnosticsDefaultError", "ErrorMsg", "#DC2626" },
    warning = { "LspDiagnosticsDefaultWarning", "WarningMsg", "#FBBF24" },
    info = { "LspDiagnosticsDefaultInformation", "#2563EB" },
    hint = { "LspDiagnosticsDefaultHint", "#10B981" },
    default = { "Identifier", "#7C3AED" },
  },
}


require("which-key").register({
  g = {
    J = {"<cmd>SplitjoinJoin<cr>", "Smart Join"},
    K = {"<cmd>SplitjoinSplit<cr>", "Smart Split"},
    R = {"<plug>(SubversiveSubstituteToEndOfLine)", "Substitute to EOL"},
    r = {"<plug>(SubversiveSubstitute)", "Substitute"},
    rR = {"<plug>(SubversiveSubstitute)^", "Substitute to SOL"},
    rr = {"<plug>(SubversiveSubstituteLine)", "Substitute Line"},
    [":"] = {"Q", "Ex Mode"},
    O = {"O<Esc>", "Insert Blankline Before"},
    o = {"o<Esc>", "Insert Blankline"},
    j = {"J", "Join"},
    k = {"i<cr><esc>", "Split"},
    t = {"<Plug>(EasyAlign)", "Easy Allign"},
    I = {"Insert at last Insertion Point"},
    i = {"<Plug>(ninja-insertstart)", "Insert in Object"},
    a = {"<Plug>(ninja-insertend)", "Append to Object"},
    c = {"Comment"},
    cc = {"Comment Line"},
    P = {
      name = "Paste Before",
      b = {"<Plug>UnconditionalPasteBlockBefore", "Paste Block"},
      B = {"<Plug>UnconditionalPasteJaggedBefore", "Paste Jagged"},
      I = {"<Plug>UnconditionalPasteCharBefore", "Paste Char"},
      i = {"<Plug>UnconditionalPasteInlinedBefore", "Paste Inline"},
      l = {"<plug>UnconditionalPasteLineBefore", "Paste Line"},
      S = {"<Plug>UnconditionalPasteParagraphedBefore", "Paste Paragraph"},
      s = {"<Plug>UnconditionalPasteSpacedBefore", "Paste Spaced"},
    },
    p = {
      name = "Paste After",
      b = {"<Plug>UnconditionalPasteBlockAfter", "Paste Block"},
      B = {"<Plug>UnconditionalPasteJaggedAfter", "Paste Jagged"},
      I = {"<Plug>UnconditionalPasteCharAfter", "Paste Char"},
      i = {"<Plug>UnconditionalPasteInlinedAfter", "Paste Inline"},
      l = {"<plug>UnconditionalPasteLineAfter", "Paste Line"},
      S = {"<Plug>UnconditionalPasteParagraphedAfter", "Paste Paragraph"},
      s = {"<Plug>UnconditionalPasteSpacedAfter", "Paste Spaced"},
    },
    s = {
      name = "Change Case",
      p = {"<Plug>CaserMixedCase", "Mixed Case"},
      c = {"<Plug>CaserCamelCase", "Camel Case"},
      ["_"] = {"<Plug>CaserSnakeCase", "Snake Case"},
      u = {"<Plug>CaserUpperCase", "Upper Case"},
      t = {"<Plug>CaserTitleCase", "Title Case"},
      d = {"<Plug>CaserSentenceCase", "Sentance Case"},
      ["<space>"] = {"<Plug>CaserSpaceCase", "Space Case"},
      ["-"] = {"<Plug>CaserKebabCase", "Kebab Case"},
      k = {"<Plug>CaserTitleKebabCase", "Title Kebab Case"},
      ["."] = {"<Plug>CaserDotCase", "Dot Case"},
      s ={
        name = "Change Case (Line)",
        p = {"gspix", "Mixed Case", noremap=false},
        c = {"gscix", "Camel Case", noremap=false},
        ["_"] = {"gs_ix", "Snake Case", noremap=false},
        u = {"gsuix", "Upper Case", noremap=false},
        t = {"gstix", "Title Case", noremap=false},
        d = {"gssix", "Sentance Case", noremap=false},
        ["<space>"] = {"gs<space>ix", "Space Case", noremap=false},
        ["-"] = {"gs-ix", "Kebab Case", noremap=false},
        k = {"gskix", "Title Kebab Case", noremap=false},
        ["."] = {"gs.ix", "Dot Case", noremap=false},
      },
      S = {
        name = "Change Case (SOL)",
        p = {"gspH", "Mixed Case", noremap=false},
        c = {"gscH", "Camel Case", noremap=false},
        ["_"] = {"gs_H", "Snake Case", noremap=false},
        u = {"gsuH", "Upper Case", noremap=false},
        t = {"gstH", "Title Case", noremap=false},
        d = {"gssH", "Sentance Case", noremap=false},
        ["<space>"] = {"gs<space>H", "Space Case", noremap=false},
        ["-"] = {"gs-H", "Kebab Case", noremap=false},
        k = {"gskH", "Title Kebab Case", noremap=false},
        ["."] = {"gs.H", "Dot Case", noremap=false},
      },
    },
    S = {
      name = "Change Case (EOL)",
      p = {"gspL", "Mixed Case", noremap=false},
      c = {"gscL", "Camel Case", noremap=false},
      ["_"] = {"gs_L", "Snake Case", noremap=false},
      u = {"gsuL", "Upper Case", noremap=false},
      t = {"gstL", "Title Case", noremap=false},
      d = {"gssL", "Sentance Case", noremap=false},
      ["<space>"] = {"gs<space>L", "Space Case", noremap=false},
      ["-"] = {"gs-L", "Kebab Case", noremap=false},
      k = {"gskL", "Title Kebab Case", noremap=false},
      ["."] = {"gs.L", "Dot Case", noremap=false},
    },
  },
  ["<leader>"] = {
    ["<leader>"] = {"<c-^>", "Alternate File"},
    ["/"] = {"<plug>(Telescope-relevant)", "Related Files"},
    [">"] = {"<cmd>Telescope spell_suggest<cr>", "Spelling Suggestions"},
    o = {
      name = "Open",
      f = {"gf", "Open File"},
    },
    F = {"<cmd>Telescope commands<cr>", "Commands"},
    f = {
      name = "Find",
      ["/"] = {"<cmd>Telescope search_history<cr>", "Search History"},
      [":"] = {"<cmd>Telescope command_history<cr>", "Search History"},
      B = {"<cmd>Telescope buffers only_cwd=true show_all_buffers=true<cr>", "Buffers (cwd)"},
      b = {"<cmd>Telescope buffers show_all_buffers=true<cr>", "Buffers"},
      C = {"<cmd>Telescope git_bcommits<cr>", "Commits (buffer)"},
      c = {"<cmd>Telescope git_commits<cr>", "Git Commits"},
      E = {"<cmd>Telescope lsp_document_diagnostics<cr>", "Errors (buffer)"},
      e = {"<cmd>Telescope lsp_workspace_diagnostics<cr>", "Errors"},
      F = {"<cmd>Telescope fd<cr>", "Files (non git)"},
      G = {"<cmd>Git! difftool<cr><cmd>cclose<cr><cmd>Telescope quickfix<cr>", "Git Chunks"},
      g = {"<cmd>Telescope git_status<cr>", "Git Status"},
      j = {"<cmd>Telescope jumps jumps<cr>", "Jumps"},
      l = {"<cmd>Telescope current_buffer_fuzzy_find<cr>", "Line"},
      m = {"<cmd>Telescope marks<cr>", "Marks"},
      n = {"<cmd>TodoTelescope<cr>", "Todo Items"},
      i = {"<cmd>Telescope media_files<cr>", "Images (and other media)"},
      o = {"<cmd>Telescope oldfiles<cr>", "Old Files"},
      Q = {"<cmd>Telescope loclist<cr>", "LocList"},
      q = {"<cmd>Telescope quickfix<cr>", "QuickFix"},
      r = {"<cmd>Telescope live_grep<cr>", "Grep"},
      R = {"<plug>(Telescope-grep)", "Fast Grep"},
      t = {"<cmd>Telescope treesitter<cr>", "Treesitter"},
      V = {"<plug>(Telescope-Vimgrep-files)", "Vim Grep (file select)"},
      v = {"<plug>(Telescope-Vimgrep-all)", "Vim Grep"},
      w = {"<plug>(Telescope-find)", "Find"},
      W = {"<plug>(Telescope-locate)", "Locate"},
      y = {"<cmd>Telescope registers<cr>", "Registers"},
    },
    G = {"<cmd>Git<cr>", "Git Status"},
    g = {
      name = "Git",
      a = {"<cmd>lua require'gitsigns'.blame_line()<CR>", "Blame Line"},
      A = {"<cmd>Gitsigns toggle_current_line_blame<CR>", "Blame Toggle"},
      b = {"<cmd>Telescope git_branches<cr>", "Branches"},
      C = {"<cmd>Telescope git_bcommits<cr>", "Commits (buffer)"},
      c = {"<cmd>Telescope git_commits <cr>", "Commits"},
      g = {"<cmd>call PMToggleView('gitdiff')<CR>", "Git Diff Viewer"},
      h = {"<cmd>GV?<CR>", "History"},
      m = {"<cmd>Git commit<cr>", "Edit Commit Message"},
      p = {"<cmd>lua require'gitsigns'.preview_hunk()<CR>", "Hunk Preview"},
      r = {"<cmd>lua require'gitsigns'.reset_hunk()<CR>", "Hunk Reset"},
      R = {"<cmd>Gitsigns reset_buffer<CR>", "Blame Toggle"},
      S = {"<cmd>Git stage %<CR>", "Stage File"},
      s = {"<cmd>lua require'gitsigns'.stage_hunk()<CR>", "Hunk Stage"},
      T = {"<cmd>GV!<CR>", "Tree (buffer)"},
      t = {"<cmd>GV<CR>", "Tree"},
      u = {"<cmd>lua require'gitsigns'.undo_stage_hunk()<CR>", "Hunk Undo"},
      v = {"<cmd>Gitsigns select_hunk<CR>", "Blame Toggle"},
    },
    p = {
      name = "Preview",
      g = {"<cmd>lua require'gitsigns'.preview_hunk()<CR>", "Hunk Preview"},
    },
    v = {
      name = "View",
      e = {"<cmd>call PMToggleView('errorlist')<CR>", "Error List"},
      E = {"<cmd>call PMToggleView('errorlistdoc')<CR>", "Error List (buffer)"},
      g = {"<cmd>call PMToggleView('gitdiff')<CR>", "Git"},
      i = {"<cmd>call PMToggleView('term')<CR>", "Terminal"},
      l = {"<cmd>call PMToggleView('loclist')<CR>", "Location List"},
      q = {"<cmd>call PMToggleView('quickfix')<CR>", "QuickFix List"},
      s = {"<cmd>call PMToggleView('symbols')<CR>", "Symbol List"},
      t = {"<cmd>call PMToggleView('nvim-tree')<CR>", "File Tree"},
      u = {"<cmd>call PMToggleView('undotree')<CR>", "Undo Tree"},
      v = {"<cmd>call CloseAllPanels()<cr>", "Close All Panels"},
    },
    Q = {"<cmd>CClear<cr><cmd>cgetbuffer<cr><cmd>TroubleRefresh<cr>", "Populater QF List With Buffer Errors "},
    q = {
      name = "QuickFix List",
      a = {"<cmd>caddbuffer<cr><cmd>TroubleRefresh<cr>", "Add Buffer Errrors to QF List"},
      c = {"<cmd>CClear<cr><cmd>TroubleRefresh<cr>", "Clear The List"},
      g = {"<cmd>Git! difftool<cr><cmd>Trouble quickfix<cr>", "Populate With Diffs"},
      n = {"<cmd>cnewer<cr>", "Newer List"},
      p = {"<cmd>colder<cr>", "Older List"},
      q = {"<cmd>call PMToggleView('quickfix')<CR>", "Open"},
      f = {"<cmd>call PMToggleView('quickfixFilter')<CR>", "Filter List"},
      V = {"<plug>(Quickfix-vimgrep-files)", "Populate With VimGrep (file select)"},
      v = {"<plug>(Quickfix-vimgrep-all)", "Populate With VimGrep"},
      w = {"<plug>(Quickfix-find)", "Populate With find"},
      W = {"<plug>(Quickfix-locate)", "Populate With Locate"},
      -- s = {"<cmd>SpellCheck!<cr>", "Populate With Spelling Errors"},
    },
    i = {
      name = "Interactive Terminal",
      i = {"<cmd>call PMToggleView('term')<CR>", "Open Terminal"},
    },
    L = {"<cmd>LClear<cr><cmd>lgetbuffer<cr><cmd>TroubleRefresh<cr>", "Populater LocList With Buffer Errors "},
    l = {
      name = "Location List",
      a = {"<cmd>laddbuffer<cr><cmd>TroubleRefresh<cr>", "Add Buffer Errrors to LocList"},
      c = {"<cmd>LClear<cr><cmd>TroubleRefresh<cr>", "Clear The List"},
      l = {"<cmd>call PMToggleView('loclist')<CR>", "Open Location List"},
      f = {"<cmd>call PMToggleView('loclistFilter')<CR>", "Filter List"},
      n = {"<cmd>lnewer<cr>", "Newer List"},
      p = {"<cmd>lolder<cr>", "Older List"},
      V = {"<plug>(Loclist-vimgrep-files)", "Populate With VimGrep (file select)"},
      v = {"<plug>(Loclist-vimgrep-all)", "Populate With VimGrep"},
      w = {"<plug>(Loclist-find)", "Populate With find"},
      W = {"<plug>(Loclist-locate)", "Populate With Locate"},
      -- s = {"<cmd>SpellLCheck!<cr>", "Populate With Spelling Errors"},
    },
    e = {
      name = "Errors",
      e = {"<cmd>call PMToggleView('errorlist')<CR>", "Open Errors"},
      E = {"<cmd>call PMToggleView('errorlistdoc')<CR>", "Open Errors (buffer)"},
      n = {"<cmd>call PMToggleView('Todo-Trouble')<cr>", "Todo Items"},
      t = {"<cmd>call PMToggleView('troubleTelescope')<CR>", "Open Telescope List"},
      r = {"<cmd>TroubleRefresh<cr>", "Refresh Errors"},
    },
    r = {
      name = "Refactor",
      s = {"<Plug>(Scalpel)", "Rename (Scalpel)"},
      t = {"Rename (Treesitter)"},
      v = {"<plug>(ExtractVar)", "Extract Variable"},
      d = {"<cmd>norm [jOdocs<tab><cr>", "Make Docstring"}
    },
    t = {
      name = "Explorer",
      e = {"<cmd>edit .<cr>", "Explorer"},
      x = {"<cmd>split .<cr>", "Horizontal Explorer"},
      v = {"<cmd>vsplit .<cr>", "Vertical Explorer"},
      t = {"<cmd>tabnew .<cr>", "Tab Explorer"},
      l = {"<cmd>Explore<cr>", "Explore Here"},
    },
    b = {
      name = "buffers",
      o = {"<cmd>Bdelete hidden<cr>", "Close All Hidden Buffers"},
    },
    w = {
      name = "Window Managment",
      w = {"<cmd>ZenMode<cr>", "Zen Mode"},
      o = {"<c-w>o", "Clean Up Windows"},
      ["<bs>"] = {"<c-w>c", "Close Window"},
      ["<cr>"] = {"<c-w>v", "Open Window"},
      x = {"<c-w>s", "Horizontal Split"},
      v = {"<c-w>v", "Vertical Split"},
      n = {"<C-W>w", "Next Window"},
      p = {"<C-W>W", "Previous Window"},
      N = {"<C-W>r", "Move Window Next"},
      P = {"<C-W>R", "Move Window Previous"},
      ["]"] = {"<cmd>vertical resize +5<cr>", "Vertical Resize"},
      ["["] = {"<cmd>vertical resize -5<cr>", "Vertical Resize"},
      ["}"] = {"<cmd>resize +5<cr>", "Horizontal Resize"},
      ["{"] = {"<cmd>resize -5<cr>", "Horizontal Resize"},
      ["="] = {"<c-w>=", "Equal Size"},
      h = {"<c-w>h", "Left Windown"},
      j = {"<c-w>j", "Below Window"},
      k = {"<c-w>k", "Above Window"},
      l = {"<c-w>l", "Right Window"},
      ["<left>"] = {"<c-w>h", "Left Windown"},
      ["<down>"] = {"<c-w>j", "Below Window"},
      ["<up>"] = {"<c-w>k", "Above Window"},
      ["<right>"] = {"<c-w>l", "Right Window"},
      H = {"<c-w>H", "Move Far Left"},
      J = {"<c-w>J", "Move Far Down"},
      K = {"<c-w>K", "Move Far Up"},
      L = {"<c-w>L", "Move Far Right"},
      ["/"] = {"<c-w>^", "Open Alternate File"},
      [","] = {"<cmd>BufferLineCyclePrev<cr>", "Previous Buffer"},
      ["."] = {"<cmd>BufferLineCycleNext<cr>", "Next Buffer"},
    },
    m = {
      name = "Make",
      m = {"<plug>(Julia-precompile)", "Precompile"},
      t = {"<plug>(Julia-test)", "Test Package"},
    },
    [","] = {
      name = "Settings",
      [","] = {"<cmd>Telescope vim_options<cr>", "Vim Options"},
      s = {"<cmd>set spell!<cr>", "Toggle Spelling"},
      k = {"<cmd>Telescope keymaps<cr>", "Keymaps"},
      c = {"<cmd>Telescope colorscheme<cr>", "Color Schemes"},
      C = {"<cmd>Telescope highlights<cr>", "Highlight Groups"},
      a = {"<cmd>Telescope autocommands<cr>", "AutoCommands"},
      f = {"<cmd>Telescope filetypes<cr>", "FileTypes"},
      h = {"<cmd>Telescope help_tags<cr>", "Help Tags"},
      m = {"<cmd>Telescope man_pages<cr>", "Man Pages"},

    },
  },
      ["1<leader>vi"] = { "<cmd>call PMToggleView('term')<CR>", "Open Terminal 1"},
      ["2<leader>vi"] = { "<cmd>call PMToggleView('2term')<CR>", "Open Terminal 2"},
      ["3<leader>vi"] = { "<cmd>call PMToggleView('3term')<CR>", "Open Terminal 3"},
      ["4<leader>vi"] = { "<cmd>call PMToggleView('4term')<CR>", "Open Terminal 4"},
      ["5<leader>vi"] = { "<cmd>call PMToggleView('5term')<CR>", "Open Terminal 5"},
      ["1<leader>ii"] = { "<cmd>call PMToggleView('term')<CR>", "Open Terminal 1"},
      ["2<leader>ii"] = { "<cmd>call PMToggleView('2term')<CR>", "Open Terminal 2"},
      ["3<leader>ii"] = { "<cmd>call PMToggleView('3term')<CR>", "Open Terminal 3"},
      ["4<leader>ii"] = { "<cmd>call PMToggleView('4term')<CR>", "Open Terminal 4"},
      ["5<leader>ii"] = { "<cmd>call PMToggleView('5term')<CR>", "Open Terminal 5"},
  ["["] = {
    name = "Backward Leader",
    L = {"<cmd>try <bar> lpfile <bar> catch /E553/ <bar> llast <bar> endtry<CR>", "Loclist File"},
    N = {"<cmd>try <bar> cpfile <bar> catch /E553/ <bar> clast <bar> endtry<CR>", "QuickFix File"},
    l = {"<cmd>try <bar> lprevious <bar> catch /E553/ <bar> llast <bar> endtry<CR>", "LocList Entry"},
    n = {"<cmd>try <bar> cprevious <bar> catch /E553/ <bar> clast <bar> endtry<CR>", "QuickFix Entry"},
    t = {"<cmd>tabprevious<cr>", "Tab"},
    b = {"<cmd>BufferLineCyclePrev", "Buffer"},
    c = {"Hunk"},
    f = {"function"},
    F = {"function (end)"},
    o = {"Block"},
    O = {"Block (end)"},
    s = {"[s", "Spelling Mistake"},
    ["["] = {"Section"},
    ["]"] = {"Section (end)"},
  },
  ["]"] = {
    name = "Forward Leader",
    L = {"<cmd>try <bar> lnfile <bar> catch /E553/ <bar> lfirst <bar> endtry<CR>", "LocList File"},
    N = {"<cmd>try <bar> cnfile <bar> catch /E553/ <bar> cfirst <bar> endtry<CR>", "QuickFix File"},
    l = {"<cmd>try <bar> lnext <bar> catch /E553/ <bar> lfirst <bar> endtry<CR>", "LocList Entry"},
    n = {"<cmd>try <bar> cnext <bar> catch /E553/ <bar> cfirst <bar> endtry<CR>", "QuickFix Entry"},
    t = {"<cmd>tabnext<cr>", "Tab"},
    b = {"<cmd>BufferLineCycleNext", "Buffer"},
    c = {"Hunk"},
    f = {"function"},
    F = {"function (end)"},
    o = {"Block"},
    O = {"Block (end)"},
    s = {"]s", "Spelling Mistake"},
    ["["] = {"Section (end)"},
    ["]"] = {"Section"},
  },
  ["<localleader>"] = {
    name = "Local Leader",
  },
})
require("which-key").register({
  ["<leader>"] = {
    r = {
      s = {"<Plug>(ScalpelVisual)", "Rename (Scalpel)"},
      v = {"<plug>(ExtractVarVisual)", "Extract Variable"}
    },
  },
  g = {
    R = {"<plug>(SubversiveSubstituteToEndOfLine)", "Substitute to EOL"},
    r = {"<plug>(SubversiveSubstitute)", "Substitute"},
    rr = {"<plug>(SubversiveSubstituteLine)", "Substitute Line"},
    rR = {"<plug>(SubversiveSubstitute)H", "Substitute to SOL"},
    j = {"J", "Join"},
    k = {"c<cr><esc>", "Split"},
    t = {"<Plug>(EasyAlign)", "Align"},
    s = {
      p = {"<Plug>CaserVMixedCase", "Mixed Case"},
      c = {"<Plug>CaserVCamelCase", "Camel Case"},
      ["_"] = {"<Plug>CaserVSnakeCase", "Snake Case"},
      u = {"<Plug>CaserVUpperCase", "Upper Case"},
      t = {"<Plug>CaserVTitleCase", "Title Case"},
      s = {"<Plug>CaserVSentenceCase", "Sentance Case"},
      ["<space>"] = {"<Plug>CaserVSpaceCase", "Space Case"},
      ["-"] = {"<Plug>CaserVKebabCase", "Kebab Case"},
      k = {"<Plug>CaserVTitleKebabCase", "Title Case"},
      ["."] = {"<Plug>CaserVDotCase", "Dot Case"},
    },
  },
  z = {
    i = {"I", "Insert"},
    a = {"A", "Append"},
  },
},{ mode = "v" })

require('numb').setup()
require('foldsigns').setup()
require("range-highlight").setup {}
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
redraw

