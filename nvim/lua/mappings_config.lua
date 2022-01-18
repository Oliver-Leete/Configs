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

-- NOTE: _, =, |, :, ^, ¬ and # are free to map

-- Mappings
-- WARNING : very experimental
-- nmap("p", "<plug>(paste-away-after)")
-- nmap("P", "<plug>(paste-away-before)")
-- nnoremap("pp", "p")
-- nnoremap("PP", "P")
nnoremap(";", ":")
nnoremap(":", ";")
xnoremap(";", ":")
xnoremap(":", ";")
onoremap(";", ":")
onoremap(":", ";")

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
-- nnoremap(",", "<cmd>WhichKey g, n<cr>")
-- xnoremap(",", "<cmd>WhichKey g, x<cr>")
xnoremap("I", "I")
xnoremap("A", "A")

nnoremap("mm", function() require'harpoon.mark'.add_file() end)
nnoremap("ma", function() require'harpoon.ui'.nav_file(1)end)
nnoremap("mr", function() require'harpoon.ui'.nav_file(2)end)
nnoremap("ms", function() require'harpoon.ui'.nav_file(3)end)
nnoremap("mt", function() require'harpoon.ui'.nav_file(4)end)
nnoremap("M", function() require'harpoon.ui'.toggle_quick_menu()end)

nmap("<c-_>", ",cc")
xmap("<c-_>", ",c")

nnoremap("£", [[:exe "let @/='" . expand("<cWORD>") . ", "<cr>]])
nmap("<c-p>", "a<c-p>")

-- inoremap( "<c-y>", [[matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)']], {nowait, expr})
-- inoremap( "<c-l>", [[matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)']], {nowait, expr})
-- snoremap( "<c-y>", [[matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)']], {nowait, expr})
-- snoremap( "<c-l>", [[matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)']], {nowait, expr})

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
vim.cmd([[augroup panelMappings
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
augroup END]])

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
nnoremap("K", function() vim.lsp.buf.hover()end)
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

nnoremap("<c-t>", "<cmd>tabedit %<cr>")
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
    nnoremap("gh", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], expr)
    nnoremap("gl", [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], expr)
    xnoremap("gg", "gg", "Buffer Top")
    xnoremap("gj", "G", "Buffer Bottom")
    xnoremap("gk", "gg", "Buffer Top")
    xnoremap("gh", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], expr)
    xnoremap("gl", [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], expr)
    onoremap("gg", "gg", "Buffer Top")
    onoremap("gj", "G", "Buffer Bottom")
    onoremap("gk", "gg", "Buffer Top")
    onoremap("gh", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], expr)
    onoremap("gl", [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], expr)

    nnoremap("gt", "H", "Window Top")
    nnoremap("gc", "M", "Window Center")
    nnoremap("gb", "L", "Window Bottom")
    xnoremap("gt", "H", "Window Top")
    xnoremap("gc", "M", "Window Bottom")
    xnoremap("gb", "L", "Window Center")
    onoremap("gt", "H", "Window Top")
    onoremap("gc", "M", "Window Bottom")
    onoremap("gb", "L", "Window Center")

    nnoremap("gf", "gf", "Open File")
    nnoremap("gF", ":edit <cfile><cr>", "Open or Create File")
    nnoremap("gx", ":!xdg-open <cfile> &<cr><cr>")

    nnoremap("gd", "<cmd>Telescope lsp_definitions theme=get_ivy<cr>", "Definitions")
    nnoremap("gr", "<cmd>Telescope lsp_references theme=get_ivy<cr>", "References")
    nnoremap("gi", "<cmd>Telescope lsp_implementations theme=get_ivy<cr>", "Implementations")
    nnoremap("go", "<cmd>Telescope lsp_type_definitions theme=get_ivy<cr>", "Object Deffinition")

    -- nmap("gp", "<plug>(paste-away-after)", "Paste After Object")
    -- nmap("gP", "<plug>(paste-away-before)", "Paste Before Object")
    -- nmap("g{", "<Plug>(ninja-insert)a", "Go Insert Left Outside")
    -- nmap("g(", "<Plug>(ninja-insert)i", "Go Insert Left Inside")
    -- nmap("g)", "<Plug>(ninja-append)i", "Go Insert Right Inside")
    -- nmap("g}", "<Plug>(ninja-append)a", "Go Insert Right Outside")

mapxName.name("G", "Goto (Select)")
    nnoremap("Gg", "vgg", "Buffer Top")
    nnoremap("Gj", "vG", "Buffer Bottom")
    nnoremap("Gk", "vgg", "Buffer Top")
    nnoremap("Gh", "v^", "Line Begining")
    nnoremap("Gl", "v$", "Line End")

    nnoremap("Gt", "vH", "Window Top")
    nnoremap("Gc", "vM", "Window Bottom")
    nnoremap("Gb", "vL", "Window Center")

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

-- mapxName.name("V", "View (Lock)")
--     nmap("Vt", "vt<cmd>WhichKey V<cr>", "Cursor On Top")
--     nmap("Vv", "vv<cmd>WhichKey V<cr>", "Centre Cursor (Vertically)")
--     nmap("Vb", "vb<cmd>WhichKey V<cr>", "Cursor On Bottom")

--     nmap("Vf", "vf<cmd>WhichKey V<cr>", "Cursor At First")
--     nmap("Vm", "vm<cmd>WhichKey V<cr>", "Centre Cursor (Horizontally)")
--     nmap("Ve", "ve<cmd>WhichKey V<cr>", "Cursor At End")

--     nmap("Vh", "vh<cmd>WhichKey V<cr>", "Scroll Left")
--     nmap("Vl", "vl<cmd>WhichKey V<cr>", "Scroll Right")
--     nmap("Vj", "vj<cmd>WhichKey V<cr>", "Scroll Down")
--     nmap("Vk", "vk<cmd>WhichKey V<cr>", "Scroll Up")

--     nmap("Vu", "vu<cmd>WhichKey V<cr>", "Half Page Up")
--     nmap("Vd", "vd<cmd>WhichKey V<cr>", "Half Page Down")

mapxName.name("z", "Select Mode")
    nmap("z{", "<m-v>{", "Left Outside")
    nmap("z(", "<m-v>(", "Left Inside")
    nmap("z)", "<m-v>)", "Right Inside")
    nmap("z}", "<m-v>}", "Right Outside")
    nmap("zi", "<m-v>i", "Inside")
    nmap("zo", "<m-v>a", "Outside")
    xmap("z{", "<esc><m-v>{", "Left Outside")
    xmap("z(", "<esc><m-v>(", "Left Inside")
    xmap("z)", "<esc><m-v>)", "Right Inside")
    xmap("z}", "<esc><m-v>}", "Right Outside")
    xmap("zi", "<esc><m-v>i", "Inside")
    xmap("zo", "<esc><m-v>a", "Outside")

mapxName.name(",", "User Commands")
nnoremap(",;", "q:", "Command Buffer")
xnoremap(",;", "q:", "Command Buffer")
nnoremap(",.", "<cmd>Telescope lsp_code_actions theme=get_cursor<CR>", "Code Actions")
xnoremap(",.", "<cmd>Telescope lsp_range_code_actions theme=get_cursor<CR>", "Code Actions (Range)")

nnoremap(",j", "m1J`1", "Join")
nnoremap(",k", "i<cr><esc>", "Split")
xnoremap(",j", "J")
xnoremap(",k", "c<cr><esc>")

mapxName.name(",c", "Comment")
nmap(",r", "<plug>(SubversiveSubstitute)", "Substitute")
xmap(",r", "<plug>(SubversiveSubstitute)", "Substitute")
nmap(",t", "<Plug>(EasyAlign)", "Easy Allign")
xmap(",t", "<Plug>(EasyAlign)", "Align")

nnoremap(",O", "O<Esc>", "Insert Blankline Before")
nnoremap(",o", "o<Esc>", "Insert Blankline")
    mapxName.name(",f", "Format")
    nnoremap(",ff", function() vim.lsp.buf.formatting_sync()end, "Format")
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

    nnoremap("<leader>F", "<cmd>Telescope resume<cr>", "Resume Picker")
    mapxName.name("<leader>f", "Telescope")
    nnoremap("<leader>f/", "<cmd>Telescope search_history theme=get_ivy<cr>", "Search History")
    nnoremap("<leader>f;", "<cmd>Telescope command_history theme=get_ivy<cr>", "Search History")
    nnoremap("<leader>fs", "<cmd>Telescope lsp_workspace_symbols theme=get_ivy<cr>", "Symbols")
    nnoremap("<leader>fS", "<cmd>Telescope lsp_document_symbols theme=get_ivy<cr>", "Symbols (buffer)")
    nnoremap("<leader>fD", "<cmd>Telescope lsp_document_diagnostics theme=get_ivy<cr>", "Errors (buffer)")
    nnoremap("<leader>fd", "<cmd>Telescope lsp_workspace_diagnostics theme=get_ivy<cr>", "Errors")
    nnoremap("<leader>fB", "<cmd>Telescope buffers only_cwd=true<cr>", "Buffers (cwd)")
    nnoremap("<leader>fb", "<cmd>Telescope buffers<cr>", "Buffers")
    nnoremap("<leader>fC", "<cmd>Telescope git_bcommits theme=get_ivy<cr>", "Commits (buffer)")
    nnoremap("<leader>fc", "<cmd>Telescope git_commits theme=get_ivy<cr>", "Git Commits")
    nnoremap("<leader>fF", function() require('telescope.builtin').find_files({find_command={'fd', '-I'}})end, "Files (non git)")
    nnoremap("<leader>ff", "<cmd>call v:lua.project_files()<cr>", "Find Files")
    nnoremap("<leader>fG", "<cmd>Gitsigns setqflist<cr><cmd>Telescope quickfix theme=get_ivy<cr>", "Git Changes")
    nnoremap("<leader>fg", "<cmd>Telescope git_status theme=get_ivy<cr>", "Git Status")
    nnoremap("<leader>fj", "<cmd>Telescope jumplist<cr>", "Jumps")
    nnoremap("<leader>fl", "<cmd>Telescope current_buffer_fuzzy_find theme=get_ivy<cr>", "Fuzzy Line")
    nnoremap("<leader>fL", '<cmd>Telescope grep_string only_sort_text=true search="" theme=get_ivy<cr>', "Fuzzy Line (Project)")
    nnoremap("<leader>fm", "<cmd>Telescope marks theme=get_ivy<cr>", "Marks")
    nnoremap("<leader>fn", "<cmd>TodoTelescope theme=get_ivy<cr>", "Todo Items")
    nnoremap("<leader>fi", "<cmd>Telescope media_files<cr>", "Images (and other media)")
    nnoremap("<leader>fo", "<cmd>Telescope oldfiles<cr>", "Old Files")
    nnoremap("<leader>fq", "<cmd>Telescope quickfix theme=get_ivy<cr>", "QuickFix")
    nnoremap("<leader>fr", "<cmd>Telescope live_grep theme=get_ivy<cr>", "Grep")
    nnoremap("<leader>fR", [["<cmd> Telescope grep_string search=" . input("Grep For > ") . " theme=get_ivy<CR>"]], "Fast Grep (with regex)", expr)
    nnoremap("<leader>fV", [["<cmd> noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj " . input("In what files? > ") . " theme=get_ivy<cr><cmd>Telescope quickfix<cr>"]], "Vim Grep (file select)", expr)
    nnoremap("<leader>fv", [["<cmd> noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj **/*  theme=get_ivy<cr><cmd>Telescope quickfix<cr>"]], "Vim Grep", expr)
    nnoremap("<leader>fx", "<cmd>Telescope file_browser respect_gitignore=false<cr>", "File Browser")
    nnoremap("<leader>fX", function() require'telescope'.extensions.file_browser.file_browser{cwd=require'telescope.utils'.buffer_dir(), respect_gitignore=false}end, "File Browser (Relative)")
    nnoremap("<leader>fI", function() require'telescope.builtin'.symbols{sources={'julia'}}end, "Insert Symbols")

    mapxName.name("<leader>l", "List Stuff")
    nnoremap("<leader>ll", "<cmd>Telescope resume<cr>", "Open List")

    mapxName.name("<leader>r", "Refactor")
    xnoremap("<leader>rf", function() require('refactoring').refactor('Extract Function')end, "Extract Function")
    xnoremap("<leader>rF", function() require('refactoring').refactor('Extract Function to File')end, "Extract Function")
    xnoremap("<leader>rv", function() require('refactoring').refactor('Extract Variable')end, "Extract Variable")
    xnoremap("<leader>ri", function() require('refactoring').refactor('Inline Variable')end, "Inline Variable")
    nnoremap("<leader>rd", function() require('refactoring').debug.printf({})end, "Inline Variable")
    xnoremap("<leader>rd", function() require('refactoring').debug.print_var({})end, "Inline Variable")
    nnoremap("<leader>rc", function() require('refactoring').debug.cleanup({})end, "Inline Variable")

    mapxName.name("<leader>w", "Window Managment")
    nnoremap("<leader>w<leader>", "<c-w>p", "Jump To Last Split")
    nnoremap("<leader>wo", "<c-w>o", "Clean Up Windows")
    nnoremap("<leader>wO", "<cmd>BDelete hidden<cr>", "Close All Hidden Buffers")
    nnoremap("<leader>wf", "<c-w>w", "Focus Floating Window")
    nnoremap("<leader>wd", "<cmd>bdelete<cr>", "Delete the current buffer")
    nnoremap("<leader>wD", "<cmd>tabclose<cr>", "Close the Current Tab")
    nnoremap("<leader>w<bs>", "<c-w>c", "Close Window")
    nnoremap("<leader>w<cr>", "<c-w>v", "Open Window")
    nnoremap("<leader>wx", "<c-w>s", "Horizontal Split")
    nnoremap("<leader>wv", "<c-w>v", "Vertical Split")
    nnoremap("<leader>wn", "<C-W>n", "Open New Window")
    nnoremap("<leader>w=", "<c-w>=", "Equal Size")
    nnoremap("<leader>wh", "<c-w>H", "Move Far Left")
    nnoremap("<leader>wj", "<c-w>J", "Move Far Down")
    nnoremap("<leader>wk", "<c-w>K", "Move Far Up")
    nnoremap("<leader>wl", "<c-w>L", "Move Far Right")
    nnoremap("<leader>wc", "<c-w>c", "Close Window")
    nnoremap("<leader>wq", "<c-w>c", "Close Window")

    mapxName.name("<leader>g", "Git")
    nnoremap("<leader>gg", "<cmd>silent !kitty @ launch --cwd=current --type=window --window-title 'LazyGit' lazygit<cr>", "LazyGit")
    nnoremap("<leader>gG", "<cmd>silent !kitty @ launch --cwd=current --type=tab --tab-title 'LazyGit' lazygit<cr>", "LazyGit")
    nnoremap("<leader>ga", function() require'gitsigns'.blame_line({full=true})end, "Blame Line")
    nnoremap("<leader>gA", "<cmd>Gitsigns toggle_current_line_blame<CR>", "Blame Toggle")

    nnoremap("<leader>gb", "<cmd>Telescope git_branches<cr>", "Branches")
    nnoremap("<leader>gC", "<cmd>Telescope git_bcommits<cr>", "Commits (buffer)")
    nnoremap("<leader>gc", "<cmd>Telescope git_commits<cr>", "Commits")
    nnoremap("<leader>gq", "<cmd>Gitsigns setqflist<cr><cmd>let g:panelRepeat='q'<cr><cmd>Trouble quickfix<cr>", "Send diffs to qf")

    nnoremap("<leader>gp", "<cmd>Gitsigns preview_hunk<CR>", "Hunk Preview")
    nnoremap("<leader>gr", "<cmd>Gitsigns reset_hunk<CR>", "Hunk Reset")
    nnoremap("<leader>gR", "<cmd>Gitsigns reset_buffer<CR>", "Reset Buffer")
    nnoremap("<leader>gS", "<cmd>Gitsigns stage_buffer<CR>", "Stage File")
    nnoremap("<leader>gs", "<cmd>Gitsigns stage_hunk<CR>", "Hunk Stage")
    nnoremap("<leader>gv", "<cmd>Gitsigns select_hunk<CR>", "Select Current Hunk")
    xnoremap("<leader>gs", "<cmd>lua require'gitsigns'.stage_hunk({vim.fn.line('.'), vim.fn.line('.')})", "Stage Hunks in Range")
    xnoremap("<leader>gr", "<cmd>lua require'gitsigns'.reset_hunk({vim.fn.line('.'), vim.fn.line('.')})", "Reset Hunks in Range")

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

        mapxName.name("<leader>gh", "GitHub")
        nnoremap("<leader>ghe", [["<cmd>RepoEdit git@github.com:" . input("What repo would you like to browse > ") . "<cr>"]], "Browse Repo", expr)
        nnoremap("<leader>ghi", "<cmd>Telescope gh issues<cr>", "Search Issues")
        nnoremap("<leader>ghp", "<cmd>Telescope gh pull_request<cr>", "Search Pull Requests")
        nnoremap("<leader>ghg", "<cmd>Telescope gh gist<cr>", "Search Gists")
        nnoremap("<leader>ghr", "<cmd>Telescope gh run<cr>", "Search GH Runs")
        mapxName.name("<leader>g,", "Git Settings")
        nnoremap("<leader>g,b", "<cmd>call v:lua.gitsign_change_base()<cr>", "Change Gitsigns Base")
        nnoremap("<leader>g,B", "<cmd>call v:lua.gitsign_bchange_base()<cr>", "Change Gitsigns Base")

    mapxName.name("<leader>p", "Preview")
    nnoremap("<leader>pa", function() require'gitsigns'.blame_line({full=true})end, "Blame Line")
    nnoremap("<leader>pp", function() vim.lsp.buf.hover({ focusable = false})end, "Documentation")
    nnoremap("<leader>ps", function() vim.lsp.buf.signature_help({ focusable = false})end, "Signature")
    nnoremap("<leader>pd", function() PeekDefinition()end, "Definition")
    nnoremap("<leader>pE", "<cmd>call v:lua.toggle_diagnostics()<cr>", "Toggle Diagnostics Shown")
    nnoremap("<leader>pe", function() vim.diagnostic.open_float(0, {border='single', scope='line', source='always'})end, "Diagnostics")
    nnoremap("<leader>pW", function() print(vim.inspect(vim.lsp.buf.list_workspace_folders()))end, "Workspace Directory")
    nnoremap("<leader>pg", function() require'gitsigns'.preview_hunk()end, "Hunk Preview")

    mapxName.name("<leader>v", "View Panels")
    nmap("v:lua.commandRepeat('<leader>v', 'panelRepeat')", "Repeat Panel", expr)
    nnoremap("<leader>ve", "<cmd>let g:panelRepeat='e'<cr><cmd>TroubleToggle lsp_workspace_diagnostics<CR>", "Error List (Workspace)")
    nnoremap("<leader>vE", "<cmd>let g:panelRepeat='E'<cr><cmd>TroubleToggle lsp_document_diagnostics<CR>", "Error List")
    nnoremap("<leader>vq", "<cmd>let g:panelRepeat='q'<cr><cmd>TroubleToggle quickfix<CR>", "QuickFix List")
    nnoremap("<leader>vn", "<cmd>let g:panelRepeat='n'<cr><cmd>TodoTrouble<cr>", "Todo List")
    nnoremap("<leader>vg", "<cmd>let g:panelRepeat='g'<cr><cmd>DiffviewOpen<CR>", "Git")
    nnoremap("<leader>vu", "<cmd>let g:panelRepeat='u'<cr><cmd>UndotreeToggle<CR>", "Undo Tree")

    mapxName.name("<leader>q", "Quickfix List")
    nnoremap("<leader>ql", "<cmd>cnewer<cr>", "Newer List")
    nnoremap("<leader>qh", "<cmd>colder<cr>", "Older List")
    nmap("<leader>qq", "<cmd>let g:panelRepeat='q'<cr><cmd>TroubleToggle quickfix<CR>", "QuickFix List")
    nnoremap("<leader>qn", "<cmd>TodoQuickFix<cr><let g:panelRepeat='q'<cr><cmd>Trouble quickfix<cr>", "Populate with todo items")

    mapxName.name("<leader>e", "Errors")
    nnoremap("<leader>ep", "<cmd>call v:lua.toggle_diagnostics()<cr>", "Toggle Diagnostics Shown")
    nnoremap("<leader>er", "<cmd>TroubleRefresh<cr>", "Refresh Errors")
    nnoremap("<leader>ee", "<cmd>let g:panelRepeat='e'<cr><cmd>TroubleToggle lsp_workspace_diagnostics<CR>", "Error List")
    nnoremap("<leader>eE", "<cmd>let g:panelRepeat='E'<cr><cmd>TroubleToggle lsp_document_diagnostics<CR>", "Error List (buffer)")
    nnoremap("<leader>en", "<cmd>let g:panelRepeat='n'<cr><cmd>TodoTrouble<cr>", "Todo List")

    mapxName.name("<leader>t", "Terminal")
    nnoremap("<leader>tt", "<cmd>silent !kittyPersistent normterm<cr>", "Normal Terminal")
    nnoremap("<leader>ti", [[<cmd>silent exe "!kittyrepl replterm " . b:replCommand<cr>]], "REPL Terminal")
    nnoremap("<leader>td", [[<cmd>silent exe "!kittyPersistent debugterm " . b:debugCommand<cr>]], "Debug Terminal")
    nnoremap("<leader>tm", "<cmd>silent !kittyPersistent maketerm<cr>", "Building Terminal")

    mapxName.name("<leader>m", "Make")
    nnoremap("<leader>mq", "<cmd>silent !kitty @ close-window --match title:maketerm<cr>", "Close Make Terms")

    mapxName.name("<leader>u", "Unit Tests")
    nnoremap("<leader>uq", "<cmd>silent !kitty @ close-window --match title:testterm<cr>", "Close Test Terms")

    mapxName.name("<leader>d", "Debugging")
    nnoremap("<leader>dd", [[<cmd>silent exe "!kittyPersistent debugterm " . b:debugCommand<cr>]], "Debug Terminal")
    nnoremap("<leader>dq", "<cmd>silent !kitty @ close-window --match title:debugterm<cr>", "Close Debug Terms")

    mapxName.name("<leader>i", "REPL")
    nnoremap("<leader>iq", "<cmd>silent !kitty @ close-window --match title:replterm<cr>", "Close REPL Terms")

    mapxName.name("<leader>r", "Refactoring")
    nnoremap("<leader>rr", function() vim.lsp.buf.rename()end, "Rename (LSP)")
    nmap("<leader>rf", "zib<cmd>lua require('refactoring').refactor('Extract Function')<cr>", "Extract Function")
    nmap("<leader>rF", "zib<cmd>lua require('refactoring').refactor('Extract Function to File')<cr>", "Extract Function")
    nmap("<leader>rv", "zi,<cmd>lua require('refactoring').refactor('Extract Variable')<cr>", "Extract Variable")
    nmap("<leader>ri", "zi,<cmd>lua require('refactoring').refactor('Inline Variable')<cr>", "Inline Variable")

    mapxName.name("<leader>,", "Settings")
    nnoremap("<leader>,,", "<cmd>Telescope vim_options<cr>", "Vim Options")
    nnoremap("<leader>,s", "<cmd>set spell!<cr>", "Toggle Spelling")
    nnoremap("<leader>,k", "<cmd>Telescope keymaps<cr>", "Keymaps")
    nnoremap("<leader>,C", "<cmd>Telescope colorscheme<cr>", "Color Schemes")
    nnoremap("<leader>,c", "<cmd>Telescope highlights<cr>", "Highlight Groups")
    nnoremap("<leader>,a", "<cmd>Telescope autocommands<cr>", "AutoCommands")
    nnoremap("<leader>,f", "<cmd>Telescope filetypes<cr>", "FileTypes")
    nnoremap("<leader>,h", "<cmd>Telescope help_tags<cr>", "Help Tags")
    nnoremap("<leader>,m", "<cmd>Telescope man_pages<cr>", "Man Pages")
    nnoremap("<leader>,d", "<cmd>call v:lua.toggle_diagnostics()<cr>", "Toggle Diagnostics Shown")
    nnoremap("<leader>,w", "<cmd>set wrap!<cr>", "Toggle Text Wraping")
    nnoremap("<leader>,/", "<cmd>let @/=''<cr>", "Clear Search")

nnoremap("<leader>ok", [[":silent !kitty @ launch --type=tab --tab-title 'Kak %:t' kak %:p +" . line(".") . ":" . col(".") . "<cr>"]], "Open file in Kak", expr)
nnoremap("<leader>ov", [[":silent !kitty @ launch --type=tab --tab-title 'Vis %:t' vis %:p<cr>"]], "Open file in Kak", expr)
nnoremap("<leader>oh", [[":silent !kitty @ launch --type=tab --tab-title 'Helix %:t' hx %:p<cr>"]], "Open file in Helix", expr)
nnoremap("<leader>z", "<cmd>ZenMode<cr>", "Zen Mode")

mapxName.name("<localleader>", "Local Leader")

-- Git Diff Bindings
if vim.api.nvim_win_get_option(0, "diff") then
    nnoremap("<leader>[", "<cmd>diffget LOCAL<cr>", "Take From Local Change")
    nnoremap("<leader>]", "<cmd>diffget REMOTE<cr>", "Take From Remote Change")
    nnoremap("<leader><leader>", "<cmd>diffget BASE<cr>", "Take From Base" )
end

imap("<c-a>", "<C-O>^")
imap("<c-e>", "<END>")
cmap("<c-a>", "<HOME>")
cmap("<c-e>", "<END>")

imap("<c-]>", "<plug>luasnip-next-choice")
smap("<c-]>", "<plug>luasnip-next-choice")

imap("<c-space>", "v:lua.cmp_toggle()", expr)
smap("<c-space>", "v:lua.cmp_toggle()", expr)
cmap("<c-space>", "v:lua.cmp_toggle()", expr)

-- imap("<Up>", "v:lua.s_tab_complete()", expr)
-- imap("<Down>", "v:lua.tab_complete()", expr)
-- smap("<Up>", "v:lua.s_tab_complete()", expr)
-- smap("<Down>", "v:lua.tab_complete()", expr)

-- inoremap("<Down>", [[<cmd>lua require("cmp").select_next_item({ behavior = require("cmp").SelectBehavior.Insert })<cr>]])
-- inoremap("<Up>", [[<cmd>lua require("cmp").select_prev_item({ behavior = require("cmp").SelectBehavior.Insert })<cr>]])
-- snoremap("<Down>", [[<cmd>lua require("cmp").select_next_item({ behavior = require("cmp").SelectBehavior.Insert })<cr>]])
-- snoremap("<Up>", [[<cmd>lua require("cmp").select_prev_item({ behavior = require("cmp").SelectBehavior.Insert })<cr>]])

noremap("<c-leftmouse>", "<cmd>Telescope lsp_definitions theme=get_ivy<cr>")
noremap("<c-rightmouse>", "gf")
