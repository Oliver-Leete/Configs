mapxName = require("mapx").setup({ global = true })
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
-- require("which-key").setup({
--     plugins = {
--         presets = {
--             operators = true,
--             motions = true,
--             text_objects = true,
--             windows = false,
--             nav = false,
--             z = false,
--             g = false,
--         },
--     },
--     operators = {
--         [",c"] = "Comments",
--         [",t"] = "Allign",
--         [",r"] = "Replace",
--         [",sp"] = "Change to Pascal Case",
--         [",sc"] = "Change to Camel Case",
--         [",ss"] = "Change to Snake Case",
--         [",su"] = "Change to Upper Case",
--         [",st"] = "Change to Title Case",
--         [",sd"] = "Change to Sentance Case",
--         [",s<space>"] = "Change to Space Case",
--         [",s-"] = "Change to Kebab Case",
--         [",sk"] = "Change to Title Kebab Case",
--         [",s."] = "Change to Dot Case",
--         ["gp"] = "Paste After",
--         ["gP"] = "Paste Before",
--     },
--     hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ ", "<plug>", "<Plug>" },
--     show_help = false,
-- })

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

nnoremap("<right>", "<cmd>bnext<cr>")
nnoremap("<left>", "<cmd>bprevious<cr>")
nnoremap("<C-right>", "<cmd>tabnext<cr>")
nnoremap("<C-left>", "<cmd>tabprevious<cr>")
nnoremap("<S-right>", "<cmd>tabnext<cr>")
nnoremap("<S-left>", "<cmd>tabprevious<cr>")

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

    nnoremap("gg", "gg")
    nnoremap("gj", "G")
    nnoremap("gk", "gg")
    nnoremap("gh", "^")
    nnoremap("gl", "$")
    xnoremap("gg", "gg")
    xnoremap("gj", "G")
    xnoremap("gk", "gg")
    xnoremap("gh", "^")
    xnoremap("gl", "$")
    onoremap("gg", "gg")
    onoremap("gj", "G")
    onoremap("gk", "gg")
    onoremap("gh", "^")
    onoremap("gl", "$")

    nnoremap("gt", "H")
    nnoremap("gc", "M")
    nnoremap("gb", "L")
    xnoremap("gt", "H")
    xnoremap("gc", "M")
    xnoremap("gb", "L")
    onoremap("gt", "H")
    onoremap("gc", "M")
    onoremap("gb", "L")

    nnoremap("gf", "gf")
    nnoremap("gF", ":edit <cfile><cr>")
    nnoremap("gx", ":!xdg-open <cfile> &<cr><cr>")

    nnoremap("gd", "<cmd>Telescope lsp_definitions theme=get_ivy<cr>")
    nnoremap("gr", "<cmd>Telescope lsp_references theme=get_ivy<cr>")
    nnoremap("gi", "<cmd>Telescope lsp_implementations theme=get_ivy<cr>")
    nnoremap("go", "<cmd>Telescope lsp_type_definitions theme=get_ivy<cr>")

    -- nmap("gp", "<plug>(paste-away-after)")
    -- nmap("gP", "<plug>(paste-away-before)")
    -- nmap("g{", "<Plug>(ninja-insert)a")
    -- nmap("g(", "<Plug>(ninja-insert)i")
    -- nmap("g)", "<Plug>(ninja-append)i")
    -- nmap("g}", "<Plug>(ninja-append)a")

    nnoremap("Gg", "vgg")
    nnoremap("Gj", "vG")
    nnoremap("Gk", "vgg")
    nnoremap("Gh", "v^")
    nnoremap("Gl", "v$")

    nnoremap("Gt", "vH")
    nnoremap("Gc", "vM")
    nnoremap("Gb", "vL")

    nnoremap("vt", "zt")
    nnoremap("vv", "zz")
    nnoremap("vc", "zz")
    nnoremap("vb", "zb")
    xnoremap("vt", "zt")
    xnoremap("vv", "zz")
    xnoremap("vc", "zz")
    xnoremap("vb", "zb")

    nnoremap("vf", "zs")
    nnoremap("vm", "<cmd>set sidescrolloff=999<cr><cmd>set sidescrolloff=0<cr>")
    nnoremap("ve", "ze")
    xnoremap("vf", "zs")
    xnoremap("vm", "<cmd>set sidescrolloff=999<cr><cmd>set sidescrolloff=0<cr>")
    xnoremap("ve", "ze")

    nnoremap("vh", "zh")
    nnoremap("vl", "zl")
    nnoremap("vj", "<c-e>")
    nnoremap("vk", "<c-y>")
    xnoremap("vh", "zh")
    xnoremap("vl", "zl")
    xnoremap("vj", "<c-e>")
    xnoremap("vk", "<c-y>")

    nnoremap("vu", "<c-u>")
    nnoremap("vd", "<c-d>")
    xnoremap("vu", "<c-u>")
    xnoremap("vd", "<c-d>")

    nnoremap("vs", "<cmd>normal! HVL<cr>")
    xnoremap("vs", "<cmd>normal! HVL<cr>")

--     nmap("Vt", "vt<cmd>WhichKey V<cr>")
--     nmap("Vv", "vv<cmd>WhichKey V<cr>")
--     nmap("Vb", "vb<cmd>WhichKey V<cr>")

--     nmap("Vf", "vf<cmd>WhichKey V<cr>")
--     nmap("Vm", "vm<cmd>WhichKey V<cr>")
--     nmap("Ve", "ve<cmd>WhichKey V<cr>")

--     nmap("Vh", "vh<cmd>WhichKey V<cr>")
--     nmap("Vl", "vl<cmd>WhichKey V<cr>")
--     nmap("Vj", "vj<cmd>WhichKey V<cr>")
--     nmap("Vk", "vk<cmd>WhichKey V<cr>")

--     nmap("Vu", "vu<cmd>WhichKey V<cr>")
--     nmap("Vd", "vd<cmd>WhichKey V<cr>")

    nmap("z{", "<m-v>{")
    nmap("z(", "<m-v>(")
    nmap("z)", "<m-v>)")
    nmap("z}", "<m-v>}")
    nmap("zi", "<m-v>i")
    nmap("zo", "<m-v>a")
    xmap("z{", "<esc><m-v>{")
    xmap("z(", "<esc><m-v>(")
    xmap("z)", "<esc><m-v>)")
    xmap("z}", "<esc><m-v>}")
    xmap("zi", "<esc><m-v>i")
    xmap("zo", "<esc><m-v>a")

nnoremap(",;", "q:")
xnoremap(",;", "q:")
nnoremap(",.", "<cmd>Telescope lsp_code_actions theme=get_cursor<CR>")
xnoremap(",.", "<cmd>Telescope lsp_range_code_actions theme=get_cursor<CR>")

nnoremap(",j", "m1J`1")
nnoremap(",k", "i<cr><esc>")
xnoremap(",j", "J")
xnoremap(",k", "c<cr><esc>")

nmap(",r", "<plug>(SubversiveSubstitute)")
xmap(",r", "<plug>(SubversiveSubstitute)")
nmap(",t", "<Plug>(EasyAlign)")
xmap(",t", "<Plug>(EasyAlign)")

nnoremap(",O", "O<Esc>")
nnoremap(",o", "o<Esc>")
    nnoremap(",ff", function() vim.lsp.buf.formatting_sync()end)
    nnoremap(",f<space>", [[<cmd>%s/\v[^^ ]\zs  / /g<cr>]])
    nnoremap(",fw", [["m1!ippar w". &textwidth . "<cr>`1"]], expr, silent)
    nnoremap(",fW", [["<cmd>%!par w" . &textwidth . "<cr>"]], expr)
    xnoremap(",ff", function() vim.lsp.buf.range_formatting()end)
    xnoremap(",f<space>", ":%s/\v[^^ ]\zs  / /g<cr>")
    xnoremap(",fw", [["!par w" . &textwidth . "<cr>"]], expr)

    nnoremap(",fl", "<cmd>left<cr>")
    nnoremap(",fc", "<cmd>center<cr>")
    nnoremap(",fr", "<cmd>right<cr>")
    xnoremap(",fl", ":left<cr>")
    xnoremap(",fc", ":center<cr>")
    xnoremap(",fr", ":right<cr>")

    nmap(",Pb", "<Plug>UnconditionalPasteBlockBefore")
    nmap(",PB", "<Plug>UnconditionalPasteJaggedBefore")
    nmap(",Pi", "<Plug>UnconditionalPasteInlinedBefore")
    nmap(",Pl", "<plug>UnconditionalPasteLineBefore")
    nmap(",PS", "<Plug>UnconditionalPasteParagraphedBefore")
    nmap(",Ps", "<Plug>UnconditionalPasteSpacedBefore")
    nnoremap(",Pu", "P`]l")

    nmap(",pb", "<Plug>UnconditionalPasteBlockAfter")
    nmap(",pB", "<Plug>UnconditionalPasteJaggedAfter")
    nmap(",pi", "<Plug>UnconditionalPasteInlinedAfter")
    nmap(",pl", "<plug>UnconditionalPasteLineAfter")
    nmap(",pS", "<Plug>UnconditionalPasteParagraphedAfter")
    nmap(",ps", "<Plug>UnconditionalPasteSpacedAfter")
    nnoremap(",pu", "p`[h")

    nnoremap(",iH", "I")
    nnoremap(",ih", "i")
    nnoremap(",il", "a")
    nnoremap(",iL", "A")

    nnoremap(",iK", "ggO")
    nnoremap(",ik", "O")
    nnoremap(",ij", "o")
    nnoremap(",iJ", "Go")

    nmap(",i{", "<Plug>(ninja-insert)a")
    nmap(",i(", "<Plug>(ninja-insert)i")
    nmap(",i)", "<Plug>(ninja-append)i")
    nmap(",i}", "<Plug>(ninja-append)a")

    nmap(",sp", "<Plug>CaserMixedCase")
    nmap(",sc", "<Plug>CaserCamelCase")
    nmap(",ss", "<Plug>CaserSnakeCase")
    nmap(",su", "<Plug>CaserUpperCase")
    nmap(",st", "<Plug>CaserTitleCase")
    nmap(",sd", "<Plug>CaserSentenceCase")
    nmap(",s<space>", "<Plug>CaserSpaceCase")
    nmap(",sk", "<Plug>CaserKebabCase")
    nmap(",sk", "<Plug>CaserTitleKebabCase")
    nmap(",s.", "<Plug>CaserDotCase")
    xmap(",fp", "<Plug>CaserVMixedCase")
    xmap(",fc", "<Plug>CaserVCamelCase")
    xmap(",f_", "<Plug>CaserVSnakeCase")
    xmap(",fu", "<Plug>CaserVUpperCase")
    xmap(",ft", "<Plug>CaserVTitleCase")
    xmap(",fs", "<Plug>CaserVSentenceCase")
    xmap(",f<space>", "<Plug>CaserVSpaceCase")
    xmap(",f-", "<Plug>CaserVKebabCase")
    xmap(",fk", "<Plug>CaserVTitleKebabCase")
    xmap(",f.", "<Plug>CaserVDotCase")

nnoremap("<leader><leader>", "<cmd>e #<cr>")
nnoremap("<leader>.", "<cmd>Telescope lsp_code_actions theme=get_cursor<CR>")
xnoremap("<leader>.", "<cmd>Telescope lsp_range_code_actions theme=get_cursor<CR>")
    nnoremap("<leader>//", "<cmd>A<cr>")
    nnoremap("<leader>/r", "<cmd>Ereadme<cr>")
        nnoremap("<leader>/nr", [[<cmd>Ereadme<cr>]])

    nnoremap("<leader>F", "<cmd>Telescope resume<cr>")
    nnoremap("<leader>f/", "<cmd>Telescope search_history theme=get_ivy<cr>")
    nnoremap("<leader>f;", "<cmd>Telescope command_history theme=get_ivy<cr>")
    nnoremap("<leader>fs", "<cmd>Telescope lsp_workspace_symbols theme=get_ivy<cr>")
    nnoremap("<leader>fS", "<cmd>Telescope lsp_document_symbols theme=get_ivy<cr>")
    nnoremap("<leader>fD", "<cmd>Telescope lsp_document_diagnostics theme=get_ivy<cr>")
    nnoremap("<leader>fd", "<cmd>Telescope lsp_workspace_diagnostics theme=get_ivy<cr>")
    nnoremap("<leader>fB", "<cmd>Telescope buffers only_cwd=true<cr>")
    nnoremap("<leader>fb", "<cmd>Telescope buffers<cr>")
    nnoremap("<leader>fC", "<cmd>Telescope git_bcommits theme=get_ivy<cr>")
    nnoremap("<leader>fc", "<cmd>Telescope git_commits theme=get_ivy<cr>")
    nnoremap("<leader>fF", function() require('telescope.builtin').find_files({find_command={'fd', '-I'}})end)
    nnoremap("<leader>ff", "<cmd>call v:lua.project_files()<cr>")
    nnoremap("<leader>fG", "<cmd>Gitsigns setqflist<cr><cmd>Telescope quickfix theme=get_ivy<cr>")
    nnoremap("<leader>fg", "<cmd>Telescope git_status theme=get_ivy<cr>")
    nnoremap("<leader>fj", "<cmd>Telescope jumplist<cr>")
    nnoremap("<leader>fl", "<cmd>Telescope current_buffer_fuzzy_find theme=get_ivy<cr>")
    nnoremap("<leader>fL", '<cmd>Telescope grep_string only_sort_text=true search="" theme=get_ivy<cr>')
    nnoremap("<leader>fm", "<cmd>Telescope marks theme=get_ivy<cr>")
    nnoremap("<leader>fn", "<cmd>TodoTelescope theme=get_ivy<cr>")
    nnoremap("<leader>fi", "<cmd>Telescope media_files<cr>")
    nnoremap("<leader>fo", "<cmd>Telescope oldfiles<cr>")
    nnoremap("<leader>fq", "<cmd>Telescope quickfix theme=get_ivy<cr>")
    nnoremap("<leader>fr", "<cmd>Telescope live_grep theme=get_ivy<cr>")
    nnoremap("<leader>fR", [["<cmd> Telescope grep_string search=" . input("Grep For > ") . " theme=get_ivy<CR>"]], expr)
    nnoremap("<leader>fV", [["<cmd> noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj " . input("In what files? > ") . " theme=get_ivy<cr><cmd>Telescope quickfix<cr>"]], expr)
    nnoremap("<leader>fv", [["<cmd> noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj **/*  theme=get_ivy<cr><cmd>Telescope quickfix<cr>"]], expr)
    nnoremap("<leader>fx", "<cmd>Telescope file_browser<cr>")
    nnoremap("<leader>fX", function() require'telescope.builtin'.file_browser{cwd=require'telescope.utils'.buffer_dir()}end)
    nnoremap("<leader>fI", function() require'telescope.builtin'.symbols{sources={'julia'}}end)

    nnoremap("<leader>ll", "<cmd>Telescope resume<cr>")

    xnoremap("<leader>rf", function() require('refactoring').refactor('Extract Function')end)
    xnoremap("<leader>rF", function() require('refactoring').refactor('Extract Function to File')end)
    xnoremap("<leader>rv", function() require('refactoring').refactor('Extract Variable')end)
    xnoremap("<leader>ri", function() require('refactoring').refactor('Inline Variable')end)

    nnoremap("<leader>w<leader>", "<c-w>p")
    nnoremap("<leader>wo", "<c-w>o")
    nnoremap("<leader>wO", "<cmd>BDelete hidden<cr>")
    nnoremap("<leader>wf", "<c-w>w")
    nnoremap("<leader>wd", "<cmd>bdelete<cr>")
    nnoremap("<leader>wD", "<cmd>tabclose<cr>")
    nnoremap("<leader>w<bs>", "<c-w>c")
    nnoremap("<leader>w<cr>", "<c-w>v")
    nnoremap("<leader>wx", "<c-w>s")
    nnoremap("<leader>wv", "<c-w>v")
    nnoremap("<leader>wn", "<C-W>n")
    nnoremap("<leader>w=", "<c-w>=")
    nnoremap("<leader>wh", "<c-w>H")
    nnoremap("<leader>wj", "<c-w>J")
    nnoremap("<leader>wk", "<c-w>K")
    nnoremap("<leader>wl", "<c-w>L")
    nnoremap("<leader>wc", "<c-w>c")
    nnoremap("<leader>wq", "<c-w>c")

    nnoremap("<leader>gg", "<cmd>silent !kitty @ launch --cwd=current --type=window --window-title 'LazyGit' lazygit<cr>")
    nnoremap("<leader>gG", "<cmd>silent !kitty @ launch --cwd=current --type=tab --tab-title 'LazyGit' lazygit<cr>")
    nnoremap("<leader>ga", function() require'gitsigns'.blame_line({full=true})end)
    nnoremap("<leader>gA", "<cmd>Gitsigns toggle_current_line_blame<CR>")

    nnoremap("<leader>gb", "<cmd>Telescope git_branches<cr>")
    nnoremap("<leader>gC", "<cmd>Telescope git_bcommits<cr>")
    nnoremap("<leader>gc", "<cmd>Telescope git_commits<cr>")
    nnoremap("<leader>gq", "<cmd>Gitsigns setqflist<cr><cmd>let g:panelRepeat='q'<cr><cmd>Trouble quickfix<cr>")

    nnoremap("<leader>gp", "<cmd>Gitsigns preview_hunk<CR>")
    nnoremap("<leader>gr", "<cmd>Gitsigns reset_hunk<CR>")
    nnoremap("<leader>gR", "<cmd>Gitsigns reset_buffer<CR>")
    nnoremap("<leader>gS", "<cmd>Gitsigns stage_buffer<CR>")
    nnoremap("<leader>gs", "<cmd>Gitsigns stage_hunk<CR>")
    nnoremap("<leader>gv", "<cmd>Gitsigns select_hunk<CR>")
    xnoremap("<leader>gs", "<cmd>lua require'gitsigns'.stage_hunk({vim.fn.line('.'), vim.fn.line('.')})")
    xnoremap("<leader>gr", "<cmd>lua require'gitsigns'.reset_hunk({vim.fn.line('.'), vim.fn.line('.')})")

        nnoremap("<leader>gdl", "<cmd>call v:lua.diff_repeat()<cr>")
        nnoremap("<leader>gdd", "<cmd>call v:lua.diff_repeat()<cr>")

        nnoremap("<leader>gdD", "<cmd>let g:DiffviewLast='DiffviewOpen'<cr><cmd>DiffviewOpen<CR>")
        nnoremap("<leader>gdc", "<cmd>call v:lua.git_commits_onechange()<cr>")
        nnoremap("<leader>gdC", "<cmd>call v:lua.git_commits_againsthead()<cr>")
        nnoremap("<leader>gdb", "<cmd>call v:lua.git_branch_dif()<cr>")
        nnoremap("<leader>gdB", "<cmd>call v:lua.git_branch_mergebase()<cr>")

        nnoremap("<leader>gdh", "<cmd>let g:DiffviewLast='DiffviewFileHistory'<cr><cmd>DiffviewFileHistory<CR>")
        nnoremap("<leader>gdH", [["<cmd>let g:DiffviewLast='DiffviewFileHistory" . getcwd() . "'<cr><cmd>DiffviewFileHistory" . getcwd() . "<CR>"]], expr)

        nnoremap("<leader>ghe", [["<cmd>RepoEdit git@github.com:" . input("What repo would you like to browse > ") . "<cr>"]], expr)
        nnoremap("<leader>ghi", "<cmd>Telescope gh issues<cr>")
        nnoremap("<leader>ghp", "<cmd>Telescope gh pull_request<cr>")
        nnoremap("<leader>ghg", "<cmd>Telescope gh gist<cr>")
        nnoremap("<leader>ghr", "<cmd>Telescope gh run<cr>")
        nnoremap("<leader>g,b", "<cmd>call v:lua.gitsign_change_base()<cr>")
        nnoremap("<leader>g,B", "<cmd>call v:lua.gitsign_bchange_base()<cr>")

    nnoremap("<leader>pa", function() require'gitsigns'.blame_line({full=true})end)
    nnoremap("<leader>pp", function() vim.lsp.buf.hover({ focusable = false})end)
    nnoremap("<leader>ps", function() vim.lsp.buf.signature_help({ focusable = false})end)
    nnoremap("<leader>pd", function() PeekDefinition()end)
    nnoremap("<leader>pE", "<cmd>call v:lua.toggle_diagnostics()<cr>")
    nnoremap("<leader>pe", function() vim.diagnostic.open_float(0, {border='single', scope='line', source='always'})end)
    nnoremap("<leader>pW", function() print(vim.inspect(vim.lsp.buf.list_workspace_folders()))end)
    nnoremap("<leader>pg", function() require'gitsigns'.preview_hunk()end)

    nmap("vv", "v:lua.commandRepeat('<leader>v', 'panelRepeat')", expr)
    nnoremap("<leader>ve", "<cmd>let g:panelRepeat='e'<cr><cmd>TroubleToggle lsp_workspace_diagnostics<CR>")
    nnoremap("<leader>vE", "<cmd>let g:panelRepeat='E'<cr><cmd>TroubleToggle lsp_document_diagnostics<CR>")
    nnoremap("<leader>vq", "<cmd>let g:panelRepeat='q'<cr><cmd>TroubleToggle quickfix<CR>")
    nnoremap("<leader>vn", "<cmd>let g:panelRepeat='n'<cr><cmd>TodoTrouble<cr>")
    nnoremap("<leader>vg", "<cmd>let g:panelRepeat='g'<cr><cmd>DiffviewOpen<CR>")
    nnoremap("<leader>vu", "<cmd>let g:panelRepeat='u'<cr><cmd>UndotreeToggle<CR>")

    nnoremap("<leader>ql", "<cmd>cnewer<cr>")
    nnoremap("<leader>qh", "<cmd>colder<cr>")
    nmap("<leader>qq", "<cmd>let g:panelRepeat='q'<cr><cmd>TroubleToggle quickfix<CR>")
    nnoremap("<leader>qn", "<cmd>TodoQuickFix<cr><let g:panelRepeat='q'<cr><cmd>Trouble quickfix<cr>")

    nnoremap("<leader>ep", "<cmd>call v:lua.toggle_diagnostics()<cr>")
    nnoremap("<leader>er", "<cmd>TroubleRefresh<cr>")
    nnoremap("<leader>ee", "<cmd>let g:panelRepeat='e'<cr><cmd>TroubleToggle lsp_workspace_diagnostics<CR>")
    nnoremap("<leader>eE", "<cmd>let g:panelRepeat='E'<cr><cmd>TroubleToggle lsp_document_diagnostics<CR>")
    nnoremap("<leader>en", "<cmd>let g:panelRepeat='n'<cr><cmd>TodoTrouble<cr>")

    nnoremap("<leader>tt", "<cmd>silent !kittyPersistent normterm<cr>")
    nnoremap("<leader>ti", [[<cmd>silent exe "!kittyrepl replterm " . b:replCommand<cr>]])
    nnoremap("<leader>td", [[<cmd>silent exe "!kittyPersistent debugterm " . b:debugCommand<cr>]])
    nnoremap("<leader>tm", "<cmd>silent !kittyPersistent maketerm<cr>")

    nnoremap("<leader>mq", "<cmd>silent !kitty @ close-window --match title:maketerm<cr>")

    nnoremap("<leader>uq", "<cmd>silent !kitty @ close-window --match title:testterm<cr>")

    nnoremap("<leader>dd", [[<cmd>silent exe "!kittyPersistent debugterm " . b:debugCommand<cr>]])
    nnoremap("<leader>dq", "<cmd>silent !kitty @ close-window --match title:debugterm<cr>")

    nnoremap("<leader>iq", "<cmd>silent !kitty @ close-window --match title:replterm<cr>")

    nnoremap("<leader>rr", function() vim.lsp.buf.rename()end)
    nmap("<leader>rf", "zib<cmd>lua require('refactoring').refactor('Extract Function')<cr>")
    nmap("<leader>rF", "zib<cmd>lua require('refactoring').refactor('Extract Function to File')<cr>")
    nmap("<leader>rv", "zi,<cmd>lua require('refactoring').refactor('Extract Variable')<cr>")
    nmap("<leader>ri", "zi,<cmd>lua require('refactoring').refactor('Inline Variable')<cr>")

    nnoremap("<leader>,,", "<cmd>Telescope vim_options<cr>")
    nnoremap("<leader>,s", "<cmd>set spell!<cr>")
    nnoremap("<leader>,k", "<cmd>Telescope keymaps<cr>")
    nnoremap("<leader>,C", "<cmd>Telescope colorscheme<cr>")
    nnoremap("<leader>,c", "<cmd>Telescope highlights<cr>")
    nnoremap("<leader>,a", "<cmd>Telescope autocommands<cr>")
    nnoremap("<leader>,f", "<cmd>Telescope filetypes<cr>")
    nnoremap("<leader>,h", "<cmd>Telescope help_tags<cr>")
    nnoremap("<leader>,m", "<cmd>Telescope man_pages<cr>")
    nnoremap("<leader>,d", "<cmd>call v:lua.toggle_diagnostics()<cr>")
    nnoremap("<leader>,w", "<cmd>set wrap!<cr>")
    nnoremap("<leader>,/", "<cmd>let @/=''<cr>")

nnoremap("<leader>ok", [[":silent !kitty @ launch --type=tab --tab-title 'Kak %:t' kak %:p +" . line(".") . ":" . col(".") . "<cr>"]], expr)
nnoremap("<leader>ov", [[":silent !kitty @ launch --type=tab --tab-title 'Vis %:t' vis %:p<cr>"]], expr)
nnoremap("<leader>z", "<cmd>ZenMode<cr>")


-- Git Diff Bindings
if vim.api.nvim_win_get_option(0, "diff") then
    nnoremap("<leader>[", "<cmd>diffget LOCAL<cr>")
    nnoremap("<leader>]", "<cmd>diffget REMOTE<cr>")
    nnoremap("<leader><leader>", "<cmd>diffget BASE<cr>" )
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
