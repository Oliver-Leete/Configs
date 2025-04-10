vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

-- --Settings
vim.opt.nrformats = vim.opt.nrformats - "octal"
vim.opt.viminfo = "'100,f1"
vim.opt.clipboard = vim.opt.clipboard + "unnamedplus"
vim.opt.mouse = "a"
vim.opt.mousemoveevent = true
vim.opt.encoding = "UTF-8"
vim.opt.scrolloff = 0
vim.opt.updatetime = 100
vim.opt.backspace = "indent,eol,start"
vim.opt.diffopt = "internal,filler,closeoff,iwhite,context:100000000,linematch:60"
vim.opt.pumheight = 20
vim.opt.spelllang = "en_gb"
vim.opt.termguicolors = true
vim.opt.hidden = true
vim.opt.shortmess = "TWAcIFs"
vim.opt.showmode = false
vim.opt.laststatus = 3
vim.opt.cmdheight = 1
vim.opt.fillchars:append({
    diff = "╱",
})
vim.opt.winborder = "rounded"

-- Saving
vim.opt.confirm = true
vim.opt.swapfile = false
vim.opt.undodir = "/home/oleete/.vim/undo//"
vim.opt.undofile = true

-- Title
vim.opt.title = true
vim.opt.titlelen = 0
vim.opt.titlestring = vim.v.servername:gsub("/tmp/", "")

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
vim.opt.list = true
vim.opt.listchars = "tab:  ,trail:·"
vim.opt.formatoptions = vim.opt.formatoptions - "cro"

-- Wrapping
vim.opt.wrap = false
vim.opt.linebreak = true
vim.opt.breakindent = true
vim.opt.breakindentopt = "shift:2"
vim.g.textwidth = 100
vim.g.wrapmargin = 0
vim.opt.smoothscroll = true

-- Folds
vim.o.foldcolumn = '0'
vim.o.foldlevel = 99
vim.o.foldlevelstart = 99
vim.o.foldenable = true

-- Numbering
vim.opt.signcolumn = "yes:1"
vim.opt.numberwidth = 3
vim.opt.number = true
vim.opt.cursorline = false

-- Splitting
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.splitkeep = "topline"

vim.g.project = vim.fs.basename(vim.fn.getcwd())

vim.diagnostic.config({
    underline = false,
    update_in_insert = false,
    severity_sort = true,
    virtual_lines = false,
    virtual_text = false,
    signs = {
        text = {
            [vim.diagnostic.severity.ERROR] = " ",
            [vim.diagnostic.severity.WARN] = " ",
            [vim.diagnostic.severity.INFO] = " ",
            [vim.diagnostic.severity.HINT] = "󰅽 ",
        },
    },
})
