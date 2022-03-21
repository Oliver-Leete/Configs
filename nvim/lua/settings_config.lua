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
vim.opt.laststatus=3
vim.opt.fillchars:append({
    horiz = '━',
    horizup = '┻',
    horizdown = '┳',
    vert = '┃',
    vertleft = '┨',
    vertright = '┣',
    verthoriz = '╋',
})

-- Saving
vim.opt.confirm = true
vim.opt.swapfile = false
vim.opt.undodir = "/home/oleete/.vim/undo//"
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
vim.opt.list = true
vim.opt.listchars = "tab:> ,trail:·"
vim.opt.formatoptions = vim.opt.formatoptions - "cro"

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
