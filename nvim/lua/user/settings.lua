--Settings
vim.opt.nrformats = vim.opt.nrformats - "octal"
vim.opt.clipboard = vim.opt.clipboard + "unnamedplus"
vim.opt.viminfo = "'100,f1"
vim.opt.mouse = "a"
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
vim.opt.cmdheight = 0
vim.opt.fillchars:append({
    horiz = '━',
    horizup = '┻',
    horizdown = '┳',
    vert = '┃',
    vertleft = '┫',
    vertright = '┣',
    verthoriz = '╋',

    diff = "╱",
})
Border = "rounded"

-- Saving
vim.opt.confirm = true
vim.opt.swapfile = false
vim.opt.undodir = "/home/oleete/.vim/undo//"
vim.opt.undofile = true
vim.g.auto_save = 1
vim.g.auto_save_silent = 1

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
vim.api.nvim_set_keymap("", "<plug>(slash-after)", "<cmd>let g:dirJumps='search'<cr>zz", { noremap = true })

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
vim.opt.signcolumn = "no"
vim.opt.numberwidth = 1
vim.opt.number = false
vim.opt.cursorline = true
vim.cmd([[call matchadd('TabLineSel', '\%101v', 203)]])

-- Splitting
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.splitkeep = "topline"

local windowPositioning = vim.api.nvim_create_augroup("windowPositioning", { clear = true })
vim.api.nvim_create_autocmd(
    { "FileType" },
    { group = windowPositioning, pattern = "help", command = ":wincmd H | vertical resize 90<cr>" }
)
vim.api.nvim_create_autocmd({ "FileType" }, { group = windowPositioning, pattern = "juliadoc", command = "wincmd H" })
vim.api.nvim_create_autocmd({ "FileType" }, { group = windowPositioning, pattern = "qf", command = "wincmd J" })

-- Yank
local LuaHighlight = vim.api.nvim_create_augroup("LuaHighlight", { clear = true })
vim.api.nvim_create_autocmd({ "TextYankPost" }, {
    group = LuaHighlight,
    callback = function() require("vim.highlight").on_yank() end,
})
