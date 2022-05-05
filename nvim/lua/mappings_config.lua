mapxName = require("mapx").setup({ global = true, whichkey = true})
local expr = mapxName.expr
local nowait = mapxName.nowait
local silent = mapxName.silent

-- Leader Mapping
vim.opt.timeoutlen = 500
vim.api.nvim_set_var("mapleader", " ")
vim.api.nvim_set_var("maplocalleader", "\\")

noremap("<BackSPACE>", "<Nop>")
noremap("<SPACE>", "<Nop>")

-- Whichkey setup
require("which-key").setup({
    plugins = {
        presets = {
            operators = true,
            motions = true,
            text_objects = true,
            windows = false,
            nav = false,
            z = false,
            g = false,
        },
    },
    operators = {
        [",c"] = "Comments",
        [",t"] = "Allign",
        [",r"] = "Replace",
        [",sp"] = "Change to Pascal Case",
        [",sc"] = "Change to Camel Case",
        [",ss"] = "Change to Snake Case",
        [",su"] = "Change to Upper Case",
        [",st"] = "Change to Title Case",
        [",sd"] = "Change to Sentance Case",
        [",s<space>"] = "Change to Space Case",
        [",s-"] = "Change to Kebab Case",
        [",sk"] = "Change to Title Kebab Case",
        [",s."] = "Change to Dot Case",
        ["gp"] = "Paste After",
        ["gP"] = "Paste Before",
    },
    hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ ", "<plug>", "<Plug>" },
    show_help = false,
})

-- Un-Mappings
noremap("v", "<nop>")
noremap("V", "<nop>")
noremap("<c-v>", "<nop>")
noremap("dd", "<nop>")
noremap("cc", "<nop>")
noremap("yy", "<nop>")
noremap("z", "<nop>")

noremap("Y", "<nop>")
noremap("C", "<nop>")
noremap("D", "<nop>")
noremap("S", "<nop>")

-- NOTE: _, =, |, :, ^ are free to map

-- Mappings
-- WARNING : very experimental
-- nmap("p", "<plug>(paste-away-after)")
-- nmap("P", "<plug>(paste-away-before)")
-- nnoremap("pp", "p")
-- nnoremap("PP", "P")
nnoremap("<m-f>", ";")
nnoremap("<m-F>", ",")
nnoremap("<m-t>", ";")
nnoremap("<m-T>", ",")
xnoremap("<m-f>", ";")
xnoremap("<m-F>", ",")
xnoremap("<m-t>", ";")
xnoremap("<m-T>", ",")

nnoremap("g<c-a>", "v<c-a>")
nnoremap("g<c-x>", "v<c-x>")
nnoremap("+", "<c-a>")
nnoremap("-", "<c-x>")
nnoremap("g+", "v<c-a>")
nnoremap("g-", "v<c-x>")
xnoremap("+", "<c-a>")
xnoremap("-", "<c-x>")
xnoremap("g+", "g<c-a>")
xnoremap("g-", "g<c-x>")

xnoremap("y", "m1y`1", nowait)
xnoremap("d", "d", nowait)
xnoremap("c", "c", nowait)

nnoremap("x", "V")
nnoremap("X", "V")
nnoremap("C", "<c-v>j")
nnoremap("<m-C>", "<c-v>k")
nnoremap("<m-C>", "<c-v>k")
nnoremap("<M-v>", "v")

xnoremap("x", "j$")
xnoremap("X", "<esc>`<kV`>")
xnoremap("C", "j")
xnoremap("<m-C>", "<esc>`<k<c-v>`>")
xnoremap("<M-v>", "v")
xnoremap("<M-;>", "o")

nnoremap("<m-c>", [["_c]])
nnoremap("<m-d>", [["_d]])
xnoremap("<m-c>", [["_c]])
xnoremap("<m-d>", [["_d]])

nnoremap("<m-o>", "m1o<esc>`1")
nnoremap("<m-O>", "m1O<esc>`1")
xnoremap("<m-o>", "<esc>`>o<esc>gv")
xnoremap("<m-O>", "<esc>`<O<esc>gv")

nnoremap("]", "<cmd>WhichKey ] n<cr>")
xnoremap("]", "<cmd>WhichKey ] x<cr>")
onoremap("]", "<cmd>WhichKey ] o<cr>")
nnoremap("[", "<cmd>WhichKey [ n<cr>")
xnoremap("[", "<cmd>WhichKey [ x<cr>")
onoremap("[", "<cmd>WhichKey [ o<cr>")
nnoremap("=", "<cmd>WhichKey = n<cr>")
xnoremap("=", "<cmd>WhichKey = x<cr>")
onoremap("=", "<cmd>WhichKey = o<cr>")
-- nnoremap(",", "<cmd>WhichKey g, n<cr>")
-- xnoremap(",", "<cmd>WhichKey g, x<cr>")
xnoremap("I", "I")
xnoremap("A", "A")

nnoremap("<leader>n", function() require'harpoon.mark'.add_file() end)
nnoremap("<leader>e", function() require("harpoon.ui").toggle_quick_menu()end)
nnoremap("<leader>a", function() require'harpoon.ui'.nav_file(1)end)
nnoremap("<leader>r", function() require'harpoon.ui'.nav_file(2)end)
nnoremap("<leader>s", function() require'harpoon.ui'.nav_file(3)end)
nnoremap("<leader>t", function() require'harpoon.ui'.nav_file(4)end)

nmap("<c-_>", ",cc")
xmap("<c-_>", ",c")

nnoremap("£", [[:exe "let @/='" . expand("<cWORD>") . ", "<cr>]])

vim.cmd([[inoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[inoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[snoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[snoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])

inoremap("<c-g>", "<c-o>%")
inoremap("<c-s>", function() require('lsp_signature').toggle_float_win()end)

inoremap(",", ",<c-g>u")
inoremap(".", ".<c-g>u")
inoremap("!", "!<c-g>u")
inoremap("?", "?<c-g>u")

-- Panel Specific Mappings
local panelMappings = vim.api.nvim_create_augroup("panelMappings", { clear = true})
vim.api.nvim_create_autocmd("filetype", {
    pattern = {"qf", "help", "vim-plug", "juliadoc", "lspinfo", "tsplayground", "harpoon-menu"},
    callback = function() vim.keymap.set("n", "<esc>", "<cmd>q<cr>", { buffer = 0 }) end,
    group = panelMappings,
})
vim.api.nvim_create_autocmd("filetype", {
    pattern = {"DiffviewFiles", "DiffviewFileHistory"},
    callback = function() vim.keymap.set("n", "<esc>", "<cmd>DiffviewClose<cr>", { buffer = 0 }) end,
    group = panelMappings,
})
vim.api.nvim_create_autocmd("filetype", {
    pattern = "undotree",
    callback = function() vim.keymap.set("n", "<esc>", "<cmd>UndotreeHide<cr>", { buffer = 0 }) end,
    group = panelMappings,
})

vim.api.nvim_set_var("wordmotion_prefix", "$")

-- UnMap Plugins
vim.api.nvim_set_var("kitty_navigator_no_mappings", true)
vim.api.nvim_set_var("UnconditionalPaste_no_mappings", true)
vim.api.nvim_set_var("caser_no_mappings	=", true)
vim.api.nvim_set_var("textobj_markdown_no_default_key_mappings", true)
vim.api.nvim_set_var("julia_blocks", false)
vim.api.nvim_set_var("surround_no_mappings", true)
vim.api.nvim_set_var("wordmotion_nomap", true)

nnoremap("j", [[v:count?(v:count>5?"m'".v:count:'').'j':'gj']], expr)
nnoremap("k", [[v:count?(v:count>5?"m'".v:count:'').'k':'gk']], expr)
xnoremap("j", [[v:count?(v:count>5?"m'".v:count:'').'j':'gj']], expr)
xnoremap("k", [[v:count?(v:count>5?"m'".v:count:'').'k':'gk']], expr)
onoremap("j", [[v:count?(v:count>5?"m'".v:count:'').'j':'gj']], expr)
onoremap("k", [[v:count?(v:count>5?"m'".v:count:'').'k':'gk']], expr)

nnoremap("H", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], expr)
nnoremap("L", [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], expr)
xnoremap("H", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], expr)
xnoremap("L", [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], expr)
onoremap("H", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], expr)
onoremap("L", [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], expr)

nnoremap("J", "gi")
nnoremap("U", "<c-r>")
xnoremap("U", "<c-r>")

xnoremap("J", ":move '>+1<cr>gv=gv")
xnoremap("K", ":move '<-2<cr>gv=gv")

nnoremap("Q", "@q")
xnoremap("Q", ":norm! @q<cr>")

nnoremap("<c-y>", "<c-r>")
nnoremap("<c-z>", "u")

nnoremap("s", function() require'hop'.hint_char1()end)
nnoremap("S", "<cmd>ISwapWith<cr>")
xnoremap("s", function() require'hop'.hint_char1({inclusive_jump=true})end)
onoremap("s", function() require'hop'.hint_char1({inclusive_jump=true})end)

nnoremap("'", "`")
nnoremap("`", "'")
xnoremap("'", "`")
xnoremap("`", "'")
onoremap("'", "`")
onoremap("`", "'")

xnoremap("<", "<gv")
xnoremap(">", ">gv")

nnoremap("<c-v>", "<cmd>vsplit %<cr>")
nnoremap("<c-x>", "<cmd>split %<cr>")

nnoremap("<cr><cr>", "<cmd>call v:lua.sendLines(v:count)<cr>", silent)
nmap("<cr>", "<plug>(sendOp)", silent)
xmap("<cr>", "<plug>(sendReg)", silent)

nmap("dp", "<plug>Dsurround")
nmap("yp", "<plug>Ysurround")
nmap("yP", "<plug>YSurround")
nmap("cp", "<plug>Csurround")
nmap("cP", "<plug>CSurround")
xmap("P", "<plug>Vsurround")

nmap("$w", "<plug>(WordMotion_w)")
nmap("$b", "<plug>(WordMotion_b)")
nmap("$e", "<plug>(WordMotion_e)")
nmap("$ge", "<plug>(WordMotion_ge)")
xmap("$w", "<plug>(WordMotion_w)")
xmap("$b", "<plug>(WordMotion_b)")
xmap("$e", "<plug>(WordMotion_e)")
xmap("$ge", "<plug>(WordMotion_ge)")
omap("$w", "<plug>(WordMotion_w)")
omap("$b", "<plug>(WordMotion_b)")
omap("$e", "<plug>(WordMotion_e)")
omap("$ge", "<plug>(WordMotion_ge)")

mapxName.name("g", "Goto")
    nnoremap("gg", "gg", "Buffer Top")
    nnoremap("gj", "G", "Buffer Bottom")
    nnoremap("gk", "gg", "Buffer Top")
    nnoremap("gh", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], "Line Start",expr)
    nnoremap("gl", [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], "Line End"  , expr)
    xnoremap("gg", "gg", "Buffer Top")
    xnoremap("gj", "G", "Buffer Bottom")
    xnoremap("gk", "gg", "Buffer Top")
    xnoremap("gh", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], "Line Start", expr)
    xnoremap("gl", [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], "Line End"  , expr)
    onoremap("gg", "gg", "Buffer Top")
    onoremap("gj", "G", "Buffer Bottom")
    onoremap("gk", "gg", "Buffer Top")
    onoremap("gh", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], "Line Start", expr)
    onoremap("gl", [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], "Line End"  , expr)

    nnoremap("gt", "H", "Window Top")
    nnoremap("gm", "M", "Window Center")
    nnoremap("gb", "L", "Window Bottom")
    xnoremap("gt", "H", "Window Top")
    xnoremap("gm", "M", "Window Bottom")
    xnoremap("gb", "L", "Window Center")
    onoremap("gt", "H", "Window Top")
    onoremap("gm", "M", "Window Bottom")
    onoremap("gb", "L", "Window Center")

    nnoremap("gf", "gf", "Open File")
    nnoremap("gF", ":edit <cfile><cr>", "Open or Create File")
    nnoremap("gx", ":!xdg-open <cfile> &<cr><cr>")

    nnoremap("gs", "<cmd>Telescope lsp_workspace_symbols theme=get_ivy<cr>", "Symbols")
    nnoremap("gS", "<cmd>Telescope lsp_document_symbols theme=get_ivy<cr>", "Symbols (Buf)")
    nnoremap("ge", "<cmd>Telescope diagnostics theme=get_ivy<cr>", "Errors")
    nnoremap("gd", "<cmd>Telescope lsp_definitions theme=get_ivy<cr>", "Definitions")
    nnoremap("gr", "<cmd>Telescope lsp_references theme=get_ivy<cr>", "References")
    nnoremap("gI", "<cmd>Telescope lsp_implementations theme=get_ivy<cr>", "Implementations")
    nnoremap("gD", "<cmd>Telescope lsp_type_definitions theme=get_ivy<cr>", "Type Deffinition")
    nnoremap("go", function() vim.lsp.buf.outgoing_calls() end, "Outgoing Calls")
    nnoremap("gi", function() vim.lsp.buf.incoming_calls() end, "Incoming Calls")

mapxName.name("v", "View")
    nnoremap("vt", "zt", "Cursor On Top")
    nnoremap("vv", "zz", "Centre Cursor (Vertically)")
    nnoremap("vc", "zz", "Centre Cursor (Vertically)")
    nnoremap("vb", "zb", "Cursor On Bottom")
    xnoremap("vt", "zt", "Cursor On Top")
    xnoremap("vv", "zz", "Centre Cursor (Vertically)")
    xnoremap("vc", "zz", "Centre Cursor (Vertically)")
    xnoremap("vb", "zb", "Cursor On Bottom")

    nnoremap("vf", "zs", "Cursor At First")
    nnoremap("vm", "<cmd>set sidescrolloff=999<cr><cmd>set sidescrolloff=0<cr>", "Centre Cursor (Horizontally)")
    nnoremap("ve", "ze", "Cursor At End")
    xnoremap("vf", "zs", "Cursor At First")
    xnoremap("vm", "<cmd>set sidescrolloff=999<cr><cmd>set sidescrolloff=0<cr>", "Centre Cursor (Horizontally)")
    xnoremap("ve", "ze", "Cursor At End")

    nnoremap("vh", "zh", "Scroll Left")
    nnoremap("vl", "zl", "Scroll Right")
    nnoremap("vj", "<c-e>", "Scroll Down")
    nnoremap("vk", "<c-y>", "Scroll Up")
    xnoremap("vh", "zh", "Scroll Left")
    xnoremap("vl", "zl", "Scroll Right")
    xnoremap("vj", "<c-e>", "Scroll Down")
    xnoremap("vk", "<c-y>", "Scroll Up")

    nnoremap("vu", "<c-u>", "Half Page Up")
    nnoremap("vd", "<c-d>", "Half Page Down")
    xnoremap("vu", "<c-u>", "Half Page Up")
    xnoremap("vd", "<c-d>", "Half Page Down")

    nnoremap("vs", "<cmd>normal! HVL<cr>", "Select Viewport")
    xnoremap("vs", "<cmd>normal! HVL<cr>", "Select Viewport")

mapxName.name("m", "Select Mode")
    nmap("mk", "<m-v>{", "Left Outside")
    nmap("mh", "<m-v>(", "Left Inside")
    nmap("ml", "<m-v>)", "Right Inside")
    nmap("mj", "<m-v>}", "Right Outside")
    nmap("mi", "<m-v>i", "Inside")
    nmap("mo", "<m-v>a", "Outside")
    xmap("mk", "<esc><m-v>{", "Left Outside")
    xmap("mh", "<esc><m-v>(", "Left Inside")
    xmap("ml", "<esc><m-v>)", "Right Inside")
    xmap("mj", "<esc><m-v>}", "Right Outside")
    xmap("mi", "<esc><m-v>i", "Inside")
    xmap("mo", "<esc><m-v>a", "Outside")

mapxName.name(",", "User Commands")
nnoremap(",;", "q:", "Command Buffer")
xnoremap(",;", "q:", "Command Buffer")
nnoremap(",.", "<cmd>Telescope lsp_code_actions theme=get_cursor<CR>", "Code Actions")
xnoremap(",.", "<cmd>Telescope lsp_range_code_actions theme=get_cursor<CR>", "Code Actions (Range)")

nnoremap(",j", "m1J`1", "Join")
nnoremap(",k", "i<cr><esc>", "Split")
xnoremap(",j", "J")
xnoremap(",k", "c<cr><esc>")

nmap("R", "<plug>(SubversiveSubstitute)", "Substitute")
xmap("R", "<plug>(SubversiveSubstitute)", "Substitute")

mapxName.name(",c", "Comment")
-- nnoremap(",r", "R", "Overwrite")
-- xnoremap(",r", "R", "Overwrite")
nnoremap(",c", function() vim.lsp.buf.rename()end, "Rename")

    -- xnoremap("<leader>ce", function() require('refactoring').refactor('Extract Function')end, "Extract Function")
    -- xnoremap("<leader>cf", function() require('refactoring').refactor('Extract Function to File')end, "Extract Function to File")
    -- xnoremap("<leader>cv", function() require('refactoring').refactor('Extract Variable')end, "Extract Variable")
    -- xnoremap("<leader>ci", function() require('refactoring').refactor('Inline Variable')end, "Inline Variable")
    -- nmap("<leader>ce", "zii<cmd>lua require('refactoring').refactor('Extract Function')<cr>", "Extract Function")
    -- nmap("<leader>cf", "zii<cmd>lua require('refactoring').refactor('Extract Function to File')<cr>", "Extract Function to File")
    -- nmap("<leader>cv", "zi,<cmd>lua require('refactoring').refactor('Extract Variable')<cr>", "Extract Variable")
    -- nmap("<leader>ci", "zi,<cmd>lua require('refactoring').refactor('Inline Variable')<cr>", "Inline Variable")
    -- nnoremap("<leader>cd", "<cmd>Neogen<cr>", "Create Docstrings")

nmap(",t", "<Plug>(EasyAlign)", "Easy Allign")
xmap(",t", "<Plug>(EasyAlign)", "Align")

nnoremap(",O", "O<Esc>", "Insert Blankline Before")
nnoremap(",o", "o<Esc>", "Insert Blankline")
    mapxName.name(",f", "Format")
    nnoremap(",ff", function() vim.lsp.buf.formatting_seq_sync()end, "Format")
    nnoremap(",f<space>", [[<cmd>%s/\v[^^ ]\zs  / /g<cr>]], "Remove Double Spaces")
    nnoremap(",fw", "m1!ippar w100<cr>`1", "Wrap Paragraph to Textwidth", silent)
    nnoremap(",fW", [["<cmd>%!par w" . &textwidth . "<cr>"]], "Wrap File to Textwidth", expr)
    xnoremap(",ff", function() vim.lsp.buf.range_formatting()end, "Format")
    xnoremap(",f<space>", ":%s/\v[^^ ]\zs  / /g<cr>", "Remove Double Spaces")
    xnoremap(",fw", [["!par w" . &textwidth . "<cr>"]], "Wrap to Textwidth", expr)

    nnoremap(",fl", "<cmd>left<cr>", "Left Allign")
    nnoremap(",fc", "<cmd>center<cr>", "Centre Allign")
    nnoremap(",fr", "<cmd>right<cr>", "Right Allign")
    xnoremap(",fl", ":left<cr>", "Left Allign")
    xnoremap(",fc", ":center<cr>", "Centre Allign")
    xnoremap(",fr", ":right<cr>", "Right Allign")

    mapxName.name(",P", "Paste Before")
    nmap(",Pb", "<Plug>UnconditionalPasteBlockBefore", "Paste Block")
    nmap(",PB", "<Plug>UnconditionalPasteJaggedBefore", "Paste Jagged")
    nmap(",Pi", "<Plug>UnconditionalPasteInlinedBefore", "Paste Inline")
    nmap(",Pl", "<plug>UnconditionalPasteLineBefore", "Paste Line")
    nmap(",PS", "<Plug>UnconditionalPasteParagraphedBefore", "Paste Paragraph")
    nmap(",Ps", "<Plug>UnconditionalPasteSpacedBefore", "Paste Spaced")
    nnoremap(",Pu", "P`]l", "Paste No Move")

    mapxName.name(",p", "Paste")
    nmap(",pb", "<Plug>UnconditionalPasteBlockAfter", "Paste Block")
    nmap(",pB", "<Plug>UnconditionalPasteJaggedAfter", "Paste Jagged")
    nmap(",pi", "<Plug>UnconditionalPasteInlinedAfter", "Paste Inline")
    nmap(",pl", "<plug>UnconditionalPasteLineAfter", "Paste Line")
    nmap(",pS", "<Plug>UnconditionalPasteParagraphedAfter", "Paste Paragraph")
    nmap(",ps", "<Plug>UnconditionalPasteSpacedAfter", "Paste Spaced")
    nnoremap(",pu", "p`[h", "Paste No Move")

    mapxName.name(",i", "Insert")
    nnoremap(",iH", "I", "Insert Before Line")
    nnoremap(",ih", "i", "Insert Before")
    nnoremap(",il", "a", "Insert After")
    nnoremap(",iL", "A", "Insert After Line")

    nnoremap(",iK", "ggO", "Insert Above File")
    nnoremap(",ik", "O", "Insert Above")
    nnoremap(",ij", "o", "Insert Below")
    nnoremap(",iJ", "Go", "Insert Below File")

    nmap(",i{", "<Plug>(ninja-insert)a", "Go Insert Left Outside")
    nmap(",i(", "<Plug>(ninja-insert)i", "Go Insert Left Inside")
    nmap(",i)", "<Plug>(ninja-append)i", "Go Insert Right Inside")
    nmap(",i}", "<Plug>(ninja-append)a", "Go Insert Right Outside")

    mapxName.name(",s", "Change Case")
    nmap(",sp", "<Plug>CaserMixedCase", "Pascal Case")
    nmap(",sc", "<Plug>CaserCamelCase", "Camel Case")
    nmap(",ss", "<Plug>CaserSnakeCase", "Snake Case")
    nmap(",su", "<Plug>CaserUpperCase", "Upper Case")
    nmap(",st", "<Plug>CaserTitleCase", "Title Case")
    nmap(",sd", "<Plug>CaserSentenceCase", "Sentance Case")
    nmap(",s<space>", "<Plug>CaserSpaceCase", "Space Case")
    nmap(",sk", "<Plug>CaserKebabCase", "Kebab Case")
    nmap(",sk", "<Plug>CaserTitleKebabCase", "Title Kebab Case")
    nmap(",s.", "<Plug>CaserDotCase", "Dot Case")
    xmap(",fp", "<Plug>CaserVMixedCase", "Pascal Case")
    xmap(",fc", "<Plug>CaserVCamelCase", "Camel Case")
    xmap(",f_", "<Plug>CaserVSnakeCase", "Snake Case")
    xmap(",fu", "<Plug>CaserVUpperCase", "Upper Case")
    xmap(",ft", "<Plug>CaserVTitleCase", "Title Case")
    xmap(",fs", "<Plug>CaserVSentenceCase", "Sentance Case")
    xmap(",f<space>", "<Plug>CaserVSpaceCase", "Space Case")
    xmap(",f-", "<Plug>CaserVKebabCase", "Kebab Case")
    xmap(",fk", "<Plug>CaserVTitleKebabCase", "Title Case")
    xmap(",f.", "<Plug>CaserVDotCase", "Dot Case")

mapxName.name("<leader>", "Leader")
nnoremap("<leader><leader>", "<cmd>e #<cr>", "Last File")
nnoremap("<leader>.", "<cmd>Telescope lsp_code_actions theme=get_cursor<CR>", "Code Actions")
xnoremap("<leader>.", "<cmd>Telescope lsp_range_code_actions theme=get_cursor<CR>", "Code Actions")
    mapxName.name("<leader>/", "Related Files")
    nnoremap("<leader>//", "<cmd>A<cr>", "Alternate File")
    nnoremap("<leader>/r", "<cmd>Ereadme<cr>", "Readme")
        mapxName.name("<leader>/n", "New File")
        nnoremap("<leader>/nr", [[<cmd>Ereadme<cr>]], "Readme")

    nnoremap("<leader>f", "<cmd>call v:lua.project_files()<cr>", "Find Files")
    nnoremap("<leader>F", "<cmd>Telescope resume<cr>", "Resume Picker")

    mapxName.name("<leader>w", "Telescope")

    mapxName.name("<leader>l", "ProfiLe")
    nnoremap("<leader>le", "<cmd>PerfPickEvent<cr>")
    nnoremap("<leader>la", "<cmd>PerfAnnotate<cr>")
    nnoremap("<leader>lf", "<cmd>PerfAnnotateFunction<cr>")
    xnoremap("<leader>la", "<cmd>PerfAnnotateSelection<cr>")
    nnoremap("<leader>lt", "<cmd>PerfToggleAnnotations<cr>")
    nnoremap("<leader>lh", "<cmd>PerfHottestLines<cr>")
    nnoremap("<leader>ls", "<cmd>PerfHottestSymbols<cr>")
    nnoremap("<leader>lc", "<cmd>PerfHottestCallersFunction<cr>")
    xnoremap("<leader>lc", "<cmd>PerfHottestCallersSelection<cr>")
        mapxName.name("<leader>lo", "Open Profile Data")
        nnoremap("<leader>lof", "<cmd>PerfLoadFlat<cr>")
        nnoremap("<leader>log", "<cmd>PerfLoadCallGraph<cr>")
        nnoremap("<leader>loo", "<cmd>PerfLoadFlameGraph<cr>")
        nnoremap("<leader>loc", "<cmd>PerfCycleFormat<cr>")

    mapxName.name("<leader>g", "Git")
    nnoremap("<leader>gb", "<cmd>Telescope git_branches<cr>", "Branches")
    nnoremap("<leader>gC", "<cmd>Telescope git_bcommits<cr>", "Commits (buffer)")
    nnoremap("<leader>gc", "<cmd>Telescope git_commits<cr>", "Commits")
    nnoremap("<leader>gq", "<cmd>Gitsigns setqflist<cr><cmd>let g:panelRepeat='q'<cr><cmd>Trouble quickfix<cr>", "Send diffs to qf")

    nnoremap("<leader>gr", "<cmd>Gitsigns reset_hunk<CR>", "Hunk Reset")
    nnoremap("<leader>gR", "<cmd>Gitsigns reset_buffer<CR>", "Reset Buffer")
    nnoremap("<leader>gS", "<cmd>Gitsigns stage_buffer<CR>", "Stage File")
    nnoremap("<leader>gs", "<cmd>Gitsigns stage_hunk<CR>", "Hunk Stage")

        mapxName.name("<leader>gd", "Git Diff")
        nnoremap("<leader>gdl", "<cmd>call v:lua.diff_repeat()<cr>", "Repeat Last Diff")
        nnoremap("<leader>gdd", "<cmd>call v:lua.diff_repeat()<cr>", "Repeat Last Diff")

        nnoremap("<leader>gdD", "<cmd>let g:DiffviewLast='DiffviewOpen'<cr><cmd>DiffviewOpen<CR>", "Git Diff Viewer")
        nnoremap("<leader>gdc", "<cmd>call v:lua.git_commits_onechange()<cr>", "View The Diff of a Commit")
        nnoremap("<leader>gdC", "<cmd>call v:lua.git_commits_againsthead()<cr>", "Diff Against a Commit")
        nnoremap("<leader>gdb", "<cmd>call v:lua.git_branch_dif()<cr>", "Diff Against a Branch")
        nnoremap("<leader>gdB", "<cmd>call v:lua.git_branch_mergebase()<cr>", "View The Diff of a Branch")

        nnoremap("<leader>gdh", "<cmd>let g:DiffviewLast='DiffviewFileHistory'<cr><cmd>DiffviewFileHistory<CR>", "View File History")
        nnoremap("<leader>gdH", [["<cmd>let g:DiffviewLast='DiffviewFileHistory" . getcwd() . "'<cr><cmd>DiffviewFileHistory" . getcwd() . "<CR>"]], "View Directory History", expr)

    mapxName.name("<leader>k", "Preview")
    nnoremap("<leader>ka", function() require'gitsigns'.blame_line({full=true})end, "Blame Line")
    nnoremap("<leader>kk", function() vim.lsp.buf.hover({ focusable = false})end, "Documentation")
    nnoremap("<leader>ks", function() vim.lsp.buf.signature_help({ focusable = false})end, "Signature")
    nnoremap("<leader>kd", function() PeekDefinition()end, "Definition")
    nnoremap("<leader>kE", "<cmd>call v:lua.toggle_diagnostics()<cr>", "Toggle Diagnostics Shown")
    nnoremap("<leader>ke", function() vim.diagnostic.open_float(0, {border='single', scope='line', source='always'})end, "Diagnostics")
    -- nnoremap("<leader>kW", function() print(vim.inspect(vim.lsp.buf.list_workspace_folders()))end, "Workspace Directory")
    nnoremap("<leader>kg", function() require'gitsigns'.preview_hunk()end, "Hunk Preview")

    mapxName.name("<leader>v", "View Panels")
    nmap("v:lua.commandRepeat('<leader>v', 'panelRepeat')", "Repeat Panel", expr)
    nnoremap("<leader>ve", "<cmd>let g:panelRepeat='e'<cr><cmd>TroubleToggle workspace_diagnostics<CR>", "Error List (Workspace)")
    nnoremap("<leader>vE", "<cmd>let g:panelRepeat='E'<cr><cmd>TroubleToggle document_diagnostics<CR>", "Error List")
    nnoremap("<leader>vq", "<cmd>let g:panelRepeat='q'<cr><cmd>TroubleToggle quickfix<CR>", "QuickFix List")

    mapxName.name("<leader>j", "Debugging")
    nnoremap("<leader>J", "<cmd>tabedit %<cr><cmd>lua require'dapui'.open()<cr>", "Open Debug Panels")
    nnoremap("<leader>jo", "<cmd>tabedit %<cr><cmd>lua require'dapui'.open()<cr>", "Open Debug Panels")
    nnoremap("<leader>jn", function() dap.step_over() end, "Step to the Next Line")
    nnoremap("<leader>jN", function() dap.continue() end, "Step to the Next Breakpoint")
    nnoremap("<leader>ji", function() dap.step_into() end, "Step In")
    nnoremap("<leader>jo", function() dap.step_out() end, "Step Out")
    nnoremap("<leader>jr", function() dap.step_back() end, "Reverse")
    nnoremap("<leader>jc", function() dap.continue() end, "Run to Cursor")
    nnoremap("<leader>jl", function() dap.repl.toggle() end, "Toggle REPL")
    nnoremap("<leader>jq", function() dap.close() end, "Quit")
    nnoremap("<leader>jb", function() dap.toggle_breakpoint() end, "Set Breakpoint")
    nnoremap("<leader>je", function() require'dapui'.eval() end, "Eval Exression")
    mapxName.name("<leader>jp", "Print Debugging")
        nnoremap("<leader>jd", function() require('refactoring').debug.printf({})end, "Printf")
        xnoremap("<leader>jd", function() require('refactoring').debug.print_var({})end, "Print Var")
        nnoremap("<leader>jq", function() require('refactoring').debug.cleanup({})end, "Cleanup")

-- Git Diff Bindings
if vim.api.nvim_win_get_option(0, "diff") then
    vim.keymap.set("n", "<leader>[", "<cmd>diffget LOCAL<cr>", "Take From Local Change")
    vim.keymap.set("n", "<leader>]", "<cmd>diffget REMOTE<cr>", "Take From Remote Change")
    vim.keymap.set("n", "<leader><leader>", "<cmd>diffget BASE<cr>", "Take From Base" )
end

-- Insert Bindings

vim.keymap.set({"i", "s", "c"}, "<c-a>", "<HOME>")
vim.keymap.set({"i", "s", "c"}, "<c-e>", "<END>")

vim.keymap.set({"i", "s"}, "<c-]>", "<plug>luasnip-next-choice")
vim.keymap.set({"i", "s", "c"}, "<c-space>", "v:lua.cmp_toggle()", {expr = true})

noremap("<c-leftmouse>", "<cmd>Telescope lsp_definitions theme=get_ivy<cr>")
noremap("<c-rightmouse>", "gf")

ls = require("luasnip")
vim.keymap.set("n", "<leader>,s", "<cmd>source ~/.config/nvim/after/plugin/luasnip.lua<cr>")
vim.keymap.set("n", "<leader>,S", "<cmd>vsplit ~/.config/nvim/after/plugin/luasnip.lua<cr>")

vim.keymap.set({ "i", "s" }, "<c-n>", function()
    if ls.expand_or_jumpable() then
      ls.expand_or_jump()
    end
end, { silent = true })

vim.keymap.set({ "i", "s" }, "<c-p>", function()
    if ls.jumpable(-1) then
      ls.jump(-1)
    end
end, { silent = true })

vim.keymap.set("i", "<c-h>", function()
    if ls.choice_active() then
        ls.change_choice(1)
    end
end)

-- Command Panel Bindings

GlobalCommands = {
    {name = "Lazygit", command = "silent !kitty @ launch --cwd=current --type=tab --tab-title 'LazyGit' lazygit"},

    {name = "Quickfix", command = "Telescope quickfix theme=get_ivy"},
    {name = "Todo List", command = "TodoTelescope theme=get_ivy"},
    {name = "Undo Tree", command = "UndotreeToggle"},

    {name = "Grep", command = "Telescope live_grep theme=get_ivy theme=get_ivy"},
    {name = "Buffer Finder", command = "Telescope buffers theme=get_ivy"},
    {name = "Old Files Finder", command = "Telescope oldfiles theme=get_ivy"},
    {name = "Symbols Finder", command = "Telescope lsp_document_symbols theme=get_ivy"},
    {name = "Workspace Symbols Finder", command = "Telescope lsp_workspace_symbols theme=get_ivy"},
    {name = "File Browser", command = "<cmd>Telescope file_browser respect_gitignore=false"},
    {name = "File Browser (Relative)", func = function() require'telescope'.extensions.file_browser.file_browser{cwd=require'telescope.utils'.buffer_dir(), respect_gitignore=false}end},

    {name = "Close Tab", command = "tabclose"},
    {name = "Zen Mode", command = "ZenMode"},
    {name = "Toggle Text Wraping", "set wrap!"},
    {name = "Clear Search", command = "let @/=''"},
}

vim.keymap.set("n", "<leader>p", function() CommandCentre() end)
function CommandCentre(argCommands)
    local commands = {}
    if argCommands == nil then
        for _, v in pairs(GlobalCommands) do
            table.insert(commands, v)
        end
        if vim.b[0].localCommands ~= nil then
            for _, v in pairs(vim.b[0].localCommands) do
                table.insert(commands, v)
            end
        end
    else
        commands = argCommands
    end

    vim.ui.select(
        commands,
        {
            prompt = "Select Commands",
            format_item = function(item)
                return item.name
            end,
        }, function(choice)

            if not choice then
                return
            end

            if choice.func ~= nil then
                choice.func()
            elseif choice.command ~= nil then
                vim.cmd(choice.command)
            elseif choice.keymap ~= nil then
                vim.api.nvim_feedkeys(
                    vim.api.nvim_replace_termcodes(choice.keymap, true, true, true),
                    "n",
                    false
                )
            else
                return
            end

        end
    )
end
