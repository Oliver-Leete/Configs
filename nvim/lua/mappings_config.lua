local m = require("mapx").setup({ global = true, whichkey = true })

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
        ["g,c"] = "Comments",
        ["g,t"] = "Allign",
        ["g,r"] = "Replace",
        ["g,sp"] = "Change to Pascal Case",
        ["g,sc"] = "Change to Camel Case",
        ["g,s_"] = "Change to Snake Case",
        ["g,su"] = "Change to Upper Case",
        ["g,st"] = "Change to Title Case",
        ["g,sd"] = "Change to Sentance Case",
        ["g,s<space>"] = "Change to Space Case",
        ["g,s-"] = "Change to Kebab Case",
        ["g,sk"] = "Change to Title Kebab Case",
        ["g,s."] = "Change to Dot Case",
        ["gp"] = "Paste After",
        ["gP"] = "Paste Before",
        ["{"] = "Goto Left Outside",
        ["("] = "Goto Left Inside",
        [")"] = "Goto Right Inside",
        ["}"] = "Goto Right Outside",
        ["g{"] = "Insert Left Outside",
        ["g("] = "Insert Left Inside",
        ["g)"] = "Insert Right Inside",
        ["g}"] = "Insert Right Outside",
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

-- NOTE: _, =, |, ^, ¬ and # are free to map

-- Mappings
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

xnoremap("y", "m1y`1", "nowait")
xnoremap("d", "d", "nowait")
xnoremap("c", "c", "nowait")

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

nnoremap("]", "<cmd>WhichKey ] n<cr>")
xnoremap("]", "<cmd>WhichKey ] x<cr>")
nnoremap("[", "<cmd>WhichKey [ n<cr>")
xnoremap("[", "<cmd>WhichKey [ x<cr>")
nnoremap(",", "<cmd>WhichKey g, n<cr>")
xnoremap(",", "<cmd>WhichKey g, x<cr>")
xnoremap("I", "I")
xnoremap("A", "A")

nnoremap("mm", "<cmd>lua require'harpoon.mark'.add_file()<cr>")
nnoremap("ma", "<cmd>lua require'harpoon.ui'.nav_file(1)<cr>")
nnoremap("mr", "<cmd>lua require'harpoon.ui'.nav_file(2)<cr>")
nnoremap("ms", "<cmd>lua require'harpoon.ui'.nav_file(3)<cr>")
nnoremap("mt", "<cmd>lua require'harpoon.ui'.nav_file(4)<cr>")
nnoremap("M", "<cmd>lua require'harpoon.ui'.toggle_quick_menu()<cr>")

nmap("<c-_>", "g,cc")
xmap("<c-_>", "g,c")

nnoremap("£", [[:exe "let @/='" . expand("<cWORD>") . ", "<cr>]])
nmap("<c-p>", "a<c-p>")

-- inoremap( "<c-y>", [[matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)']], {"nowait", "expr"})
-- inoremap( "<c-l>", [[matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)']], {"nowait", "expr"})
-- snoremap( "<c-y>", [[matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)']], {"nowait", "expr"})
-- snoremap( "<c-l>", [[matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)']], {"nowait", "expr"})

vim.cmd([[inoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[inoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[snoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[snoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])

inoremap("<c-g>", "<c-o>%")
inoremap("<c-s>", "<cmd>lua require('lsp_signature').toggle_float_win()<CR>")

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

nnoremap("j", [[v:count?(v:count>5?"m'".v:count:'').'j':'gj']], "expr")
nnoremap("k", [[v:count?(v:count>5?"m'".v:count:'').'k':'gk']], "expr")
xnoremap("j", [[v:count?(v:count>5?"m'".v:count:'').'j':'gj']], "expr")
xnoremap("k", [[v:count?(v:count>5?"m'".v:count:'').'k':'gk']], "expr")
onoremap("j", [[v:count?(v:count>5?"m'".v:count:'').'j':'gj']], "expr")
onoremap("k", [[v:count?(v:count>5?"m'".v:count:'').'k':'gk']], "expr")

nnoremap("H", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], "expr")
nnoremap("L", [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], "expr")
xnoremap("H", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], "expr")
xnoremap("L", [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], "expr")
onoremap("H", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], "expr")
onoremap("L", [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], "expr")

nnoremap("J", "gi")
nnoremap("U", "<c-r>")
xnoremap("U", "<c-r>")

xnoremap("J", ":move '>+1<cr>gv=gv")
xnoremap("K", ":move '<-2<cr>gv=gv")

nnoremap("Q", "@q")
xnoremap("Q", ":norm! @q<cr>")

nnoremap("s", "<cmd>lua require'hop'.hint_char1()<cr>")
nnoremap("S", "<cmd>ISwapWith<cr>")
xnoremap("s", "<cmd>lua require'hop'.hint_char1()<cr>")
xnoremap("S", ":lua require('tsht').nodes()<CR>")
onoremap("s", "<cmd>lua require'hop'.hint_char1()<cr>")
onoremap("S", ":<c-u>lua require('tsht').nodes()<cr>")

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

nmap("n", "v:lua.commandRepeat(']', 'dirJumps')", "expr")
vmap("n", "v:lua.commandRepeat(']', 'dirJumps')", "expr")
nmap("N", "v:lua.commandRepeat('[', 'dirJumps')", "expr")
vmap("N", "v:lua.commandRepeat('[', 'dirJumps')", "expr")

nnoremap("<cr><cr>", "<cmd>call v:lua.sendLines(v:count)<cr>")
nnoremap("<cr>", "<plug>(sendOp)")
xnoremap("<cr>", "<plug>(sendReg)")

m.name("g", "Goto")
    nnoremap("gg", "gg", "Buffer Top")
    nnoremap("gj", "G", "Buffer Bottom")
    nnoremap("gk", "gg", "Buffer Top")
    nnoremap("gh", "^", "Line Begining")
    nnoremap("gl", "$", "Line End")
    xnoremap("gg", "gg", "Buffer Top")
    xnoremap("gj", "G", "Buffer Bottom")
    xnoremap("gk", "gg", "Buffer Top")
    xnoremap("gh", "^", "Line Begining")
    xnoremap("gl", "$", "Line End")
    onoremap("gg", "gg", "Buffer Top")
    onoremap("gj", "G", "Buffer Bottom")
    onoremap("gk", "gg", "Buffer Top")
    onoremap("gh", "^", "Line Begining")
    onoremap("gl", "$", "Line End")

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
    nnoremap("gF", "<cmd>vsplit<cr>gf", "Open File in Split")

    nnoremap("gd", "<cmd>lua require('telescope.builtin').lsp_definitions()<cr>", "Definitions")
    nnoremap("gr", "<cmd>Telescope lsp_references<cr>", "References")
    nnoremap("gi", "<cmd>lua require('telescope.builtin').lsp_implementations()<CR>", "Implementations")
    nnoremap("go", "<cmd>lua require('telescope.builtin').lsp_type_definitions()<cr>", "Object Deffinition")
    nnoremap("gD", "<cmd>lua require('telescope.builtin').lsp_definitions({jump_type='vsplit'})<cr>", "Definitions (split)")
    nnoremap("gR", "<cmd>lua require('telescope.builtin').lep_references({jump_type='vsplit'})<cr>", "References (split)")
    nnoremap("gI", "<cmd>lua require('telescope.builtin').lsp_implementations({jump_type='vsplit'})<CR>", "Implementations (split)")
    nnoremap("gO", "<cmd>lua require('telescope.builtin').lsp_type_definitions({jump_type='vsplit'})<cr>", "Object Deffinition (split)")

    nnoremap("gp", "<plug>(paste-away-after)", "Paste After Object")
    nnoremap("gP", "<plug>(paste-away-before)", "Paste Before Object")
    nnoremap("g{", "<Plug>(ninja-insert)a", "Go Insert Left Outside")
    nnoremap("g(", "<Plug>(ninja-insert)i", "Go Insert Left Inside")
    nnoremap("g)", "<Plug>(ninja-append)i", "Go Insert Right Inside")
    nnoremap("g}", "<Plug>(ninja-append)a", "Go Insert Right Outside")

m.name("G", "Goto (Select)")
    nnoremap("Gg", "vgg", "Buffer Top")
    nnoremap("Gj", "vG", "Buffer Bottom")
    nnoremap("Gk", "vgg", "Buffer Top")
    nnoremap("Gh", "v^", "Line Begining")
    nnoremap("Gl", "v$", "Line End")

    nnoremap("Gt", "vH", "Window Top")
    nnoremap("Gc", "vM", "Window Bottom")
    nnoremap("Gb", "vL", "Window Center")

m.name("v", "View")
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

m.name("V", "View (Lock)")
    nmap("Vt", "vt<cmd>WhichKey V<cr>", "Cursor On Top")
    nmap("Vv", "vv<cmd>WhichKey V<cr>", "Centre Cursor (Vertically)")
    nmap("Vb", "vb<cmd>WhichKey V<cr>", "Cursor On Bottom")

    nmap("Vf", "vf<cmd>WhichKey V<cr>", "Cursor At First")
    nmap("Vm", "vm<cmd>WhichKey V<cr>", "Centre Cursor (Horizontally)")
    nmap("Ve", "ve<cmd>WhichKey V<cr>", "Cursor At End")

    nmap("Vh", "vh<cmd>WhichKey V<cr>", "Scroll Left")
    nmap("Vl", "vl<cmd>WhichKey V<cr>", "Scroll Right")
    nmap("Vj", "vj<cmd>WhichKey V<cr>", "Scroll Down")
    nmap("Vk", "vk<cmd>WhichKey V<cr>", "Scroll Up")

    nmap("Vu", "vu<cmd>WhichKey V<cr>", "Half Page Up")
    nmap("Vd", "vd<cmd>WhichKey V<cr>", "Half Page Down")

m.name("z", "Select Mode")
    nmap("zh", "<m-v>{", "Left Outside")
    nmap("zn", "<m-v>(", "Left Inside")
    nmap("ze", "<m-v>)", "Right Inside")
    nmap("zl", "<m-v>}", "Right Outside")
    xmap("zh", "<esc><m-v>{", "Left Outside")
    xmap("zn", "<esc><m-v>(", "Left Inside")
    xmap("ze", "<esc><m-v>)", "Right Inside")
    xmap("zl", "<esc><m-v>}", "Right Outside")

    nmap("z{", "<m-v>{", "Left Outside")
    nmap("z(", "<m-v>(", "Left Inside")
    nmap("z)", "<m-v>)", "Right Inside")
    nmap("z}", "<m-v>}", "Right Outside")

    nmap("zi", "<m-v>i", "Inside")
    nmap("zo", "<m-v>a", "Outside")
    xmap("zi", "<esc><m-v>i", "Inside")
    xmap("zo", "<esc><m-v>a", "Outside")

-- Normal Bindings
require("which-key").register({
    ["g,"] = {
        name = "User Commands",

        [";"] = { "q:", "Command Buffer" },
        ["."] = { "<cmd>Telescope lsp_code_actions theme=get_cursor<CR>", "Code Actions" },

        j = { "m1J`1", "Join" },
        k = { "i<cr><esc>", "Split" },

        r = { "<plug>(SubversiveSubstitute)", "Substitute" },
        t = { "<Plug>(EasyAlign)", "Easy Allign" },
        c = { "Comment" },
        O = { "O<Esc>", "Insert Blankline Before" },
        o = { "o<Esc>", "Insert Blankline" },

        f = {
            name = "Formatting",

            f = { "<cmd>lua vim.lsp.buf.formatting_sync()<CR>", "Format" },
            ["<space>"] = { [[<cmd>%s/\v[^^ ]\zs  / /g<cr>]], "Remove Double Spaces" },
            w = { [["m1!ippar w". &textwidth . "<cr>`1"]], "Wrap Paragraph to Textwidth", expr = true },
            W = { [["<cmd>%!par w" . &textwidth . "<cr>"]], "Wrap File to Textwidth", expr = true },

            l = { "<cmd>left<cr>", "Left Allign" },
            c = { "<cmd>center<cr>", "Centre Allign" },
            r = { "<cmd>right<cr>", "Right Allign" },
        },

        i = {
            name = "insert",

            H = { "I", "Insert Before Line" },
            h = { "i", "Insert Before" },
            l = { "a", "Insert After" },
            L = { "A", "Insert After Line" },

            K = { "ggO", "Insert Above File" },
            k = { "O", "Insert Above" },
            j = { "o", "Insert Below" },
            J = { "Go", "Insert Below File" },

            ["{"] = { "<Plug>(ninja-insert)a", "Go Insert Left Outside" },
            ["("] = { "<Plug>(ninja-insert)i", "Go Insert Left Inside" },
            [")"] = { "<Plug>(ninja-append)i", "Go Insert Right Inside" },
            ["}"] = { "<Plug>(ninja-append)a", "Go Insert Right Outside" },
        },

        P = {
            name = "Paste Before",

            b = { "<Plug>UnconditionalPasteBlockBefore", "Paste Block" },
            B = { "<Plug>UnconditionalPasteJaggedBefore", "Paste Jagged" },
            L = { "<Plug>UnconditionalPasteInlinedBefore", "Paste Inline" },
            l = { "<plug>UnconditionalPasteLineBefore", "Paste Line" },
            S = { "<Plug>UnconditionalPasteParagraphedBefore", "Paste Paragraph" },
            s = { "<Plug>UnconditionalPasteSpacedBefore", "Paste Spaced" },
            u = { "P`]l", "Paste No Move" },
        },

        p = {
            name = "Paste After",

            b = { "<Plug>UnconditionalPasteBlockAfter", "Paste Block" },
            B = { "<Plug>UnconditionalPasteJaggedAfter", "Paste Jagged" },
            L = { "<Plug>UnconditionalPasteInlinedAfter", "Paste Inline" },
            l = { "<plug>UnconditionalPasteLineAfter", "Paste Line" },
            S = { "<Plug>UnconditionalPasteParagraphedAfter", "Paste Paragraph" },
            s = { "<Plug>UnconditionalPasteSpacedAfter", "Paste Spaced" },
            u = { "p`[h", "Paste No Move" },
        },

        s = {
            name = "Change Case",

            p = { "<Plug>CaserMixedCase", "Pascal Case" },
            c = { "<Plug>CaserCamelCase", "Camel Case" },
            ["_"] = { "<Plug>CaserSnakeCase", "Snake Case" },
            u = { "<Plug>CaserUpperCase", "Upper Case" },
            t = { "<Plug>CaserTitleCase", "Title Case" },
            d = { "<Plug>CaserSentenceCase", "Sentance Case" },
            ["<space>"] = { "<Plug>CaserSpaceCase", "Space Case" },
            ["-"] = { "<Plug>CaserKebabCase", "Kebab Case" },
            k = { "<Plug>CaserTitleKebabCase", "Title Kebab Case" },
            ["."] = { "<Plug>CaserDotCase", "Dot Case" },
        },
    },
    ["<leader>"] = {
        ["<leader>"] = { "<cmd>e #<cr>", "Last File" },
        ["."] = { "<cmd>Telescope lsp_code_actions theme=get_cursor<CR>", "Code Actions" },
        ["/"] = {
            name = "Related Files",
            ["<leader>"] = { "<c-^>", "Last File" },
            ["/"] = { "<cmd>A<cr>", "Alternate File" },
            f = {
                [["<cmd>lua require'telescope.builtin'.find_files({find_command={'fd', \"" . split(expand("%:t:r"), '_')[0] . "\"}})<cr>"]],
                "Search",
                expr = true,
            },
            r = { "<cmd>Ereadme<cr>", "Readme" },
            v = {
                name = "Vertical Split",
                ["<leader>"] = { "<cmd>vsplit #<cr>", "Last File" },
                ["/"] = { "<cmd>AV<cr>", "Alternate File" },
                r = { "<cmd>Vreadme<cr>", "Readme" },
            },
            x = {
                name = "Horizontal Split",
                ["<leader>"] = { "<cmd>split #<cr>", "Last File" },
                ["/"] = { "<cmd>AS<cr>", "Alternate File" },
                r = { "<cmd>Sreadme<cr>", "Readme" },
            },
            O = {
                name = "New Tab",
                ["<leader>"] = { "<cmd>tabedit #<cr>", "Last File" },
                ["/"] = { "<cmd>AT<cr>", "Alternate File" },
                r = { "<cmd>Treadme<cr>", "Readme" },
            },
            n = {
                name = "New File",
                r = { [[<cmd>Ereadme<cr>]], "Readme" },
            },
        },
        f = {
            name = "Find",
            ["/"] = { "<cmd>Telescope search_history<cr>", "Search History" },
            [";"] = { "<cmd>Telescope command_history<cr>", "Search History" },
            ["*"] = {
                [[<cmd>lua require'telescope.builtin'.find_files({find_command={'rg', vim.fn.expand("<cword>")}})<cr>]],
                "Grep Word Under Cursor",
            },
            s = { "<cmd>Telescope lsp_workspace_symbols<cr>", "Symbols" },
            S = { "<cmd>Telescope lsp_document_symbols<cr>", "Symbols (buffer)" },
            E = { "<cmd>Telescope lsp_document_diagnostics<cr>", "Errors (buffer)" },
            e = { "<cmd>Telescope lsp_workspace_diagnostics<cr>", "Errors" },
            B = { "<cmd>Telescope buffers only_cwd=true<cr>", "Buffers (cwd)" },
            b = { "<cmd>Telescope buffers<cr>", "Buffers" },
            C = { "<cmd>Telescope git_bcommits<cr>", "Commits (buffer)" },
            c = { "<cmd>Telescope git_commits<cr>", "Git Commits" },
            F = { "<cmd>lua require('telescope.builtin').find_files({find_command={'fd', '-I'}})<cr>", "Files (non git)" },
            f = { "<cmd>call v:lua.project_files()<cr>", "Find Files" },
            G = { "<cmd>Gitsigns setqflist<cr><cmd>Telescope quickfix<cr>", "Git Changes" },
            g = { "<cmd>Telescope git_status<cr>", "Git Status" },
            j = { "<cmd>Telescope jumplist<cr>", "Jumps" },
            l = { "<cmd>Telescope current_buffer_fuzzy_find<cr>", "Fuzzy Line" },
            L = { '<cmd>Telescope grep_string only_sort_text=true search=""<cr>', "Fuzzy Line (Project)" },
            m = { "<cmd>Telescope marks<cr>", "Marks" },
            n = { "<cmd>TodoTelescope<cr>", "Todo Items" },
            i = { "<cmd>Telescope media_files<cr>", "Images (and other media)" },
            o = { "<cmd>Telescope oldfiles<cr>", "Old Files" },
            q = { "<cmd>Telescope quickfix<cr>", "QuickFix" },
            r = { "<cmd>Telescope live_grep<cr>", "Grep" },
            R = {
                [["<cmd> Telescope grep_string search=" . input("Grep For > ") . "<CR>"]],
                "Fast Grep (with regex)",
                expr = true,
            },
            V = {
                [["<cmd> noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj " . input("In what files? > ") . "<cr><cmd>Telescope quickfix<cr>"]],
                "Vim Grep (file select)",
                expr = true,
            },
            v = {
                [["<cmd> noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj **/* <cr><cmd>Telescope quickfix<cr>"]],
                "Vim Grep",
                expr = true,
            },
            x = { "<cmd>Telescope file_browser<cr>", "File Browser" },
            X = {
                "<cmd>lua require'telescope.builtin'.file_browser{cwd=require'telescope.utils'.buffer_dir()}<cr>",
                "File Browser (Relative)",
            },
            I = { "<cmd>lua require'telescope.builtin'.symbols{sources={'julia'}}<cr>", "Insert Symbols" },
        },
        g = {
            name = "Git",
            g = {
                "<cmd>silent !kitty @ launch --cwd=current --type=window --window-title 'LazyGit' lazygit<cr>",
                "LazyGit",
            },
            G = { "<cmd>silent !kitty @ launch --cwd=current --type=tab --tab-title 'LazyGit' lazygit<cr>", "LazyGit" },

            a = { "<cmd>lua require'gitsigns'.blame_line({full=true})<CR>", "Blame Line" },
            A = { "<cmd>Gitsigns toggle_current_line_blame<CR>", "Blame Toggle" },

            b = { "<cmd>Telescope git_branches<cr>", "Branches" },
            C = { "<cmd>Telescope git_bcommits<cr>", "Commits (buffer)" },
            c = { "<cmd>Telescope git_commits<cr>", "Commits" },
            q = {
                "<cmd>Gitsigns setqflist<cr><cmd>let g:panelRepeat='q'<cr><cmd>Trouble quickfix<cr>",
                "Send diffs to qf",
            },

            d = {
                l = { "<cmd>call v:lua.diff_repeat()<cr>", "Repeat Last Diff" },
                d = { "<cmd>call v:lua.diff_repeat()<cr>", "Repeat Last Diff" },

                D = { "<cmd>let g:DiffviewLast='DiffviewOpen'<cr><cmd>DiffviewOpen<CR>", "Git Diff Viewer" },
                c = { "<cmd>call v:lua.git_commits_onechange()<cr>", "View The Diff of a Commit" },
                C = { "<cmd>call v:lua.git_commits_againsthead()<cr>", "Diff Against a Commit" },
                b = { "<cmd>call v:lua.git_branch_dif()<cr>", "Diff Against a Branch" },
                B = { "<cmd>call v:lua.git_branch_mergebase()<cr>", "View The Diff of a Branch" },

                h = {
                    "<cmd>let g:DiffviewLast='DiffviewFileHistory'<cr><cmd>DiffviewFileHistory<CR>",
                    "View File History",
                },
                H = {
                    [["<cmd>let g:DiffviewLast='DiffviewFileHistory" . getcwd() . "'<cr><cmd>DiffviewFileHistory" . getcwd() . "<CR>"]],
                    "View Directory History",
                    expr = true,
                },
            },

            p = { "<cmd>Gitsigns preview_hunk<CR>", "Hunk Preview" },
            r = { "<cmd>Gitsigns reset_hunk<CR>", "Hunk Reset" },
            R = { "<cmd>Gitsigns reset_buffer<CR>", "Reset Buffer" },
            S = { "<cmd>Gitsigns stage_buffer<CR>", "Stage File" },
            s = { "<cmd>Gitsigns stage_hunk<CR>", "Hunk Stage" },

            v = { "<cmd>Gitsigns select_hunk<CR>", "Select Current Hunk" },

            h = {
                name = "GitHub",
                e = {
                    [["<cmd>RepoEdit git@github.com:" . input("What repo would you like to browse > ") . "<cr>"]],
                    "Browse Repo",
                    expr = true,
                },
                i = { "<cmd>Telescope gh issues<cr>", "Search Issues" },
                p = { "<cmd>Telescope gh pull_request<cr>", "Search Pull Requests" },
                g = { "<cmd>Telescope gh gist<cr>", "Search Gists" },
                r = { "<cmd>Telescope gh run<cr>", "Search GH Runs" },
            },
            [","] = {
                name = "Git Settings",
                b = { "<cmd>call v:lua.gitsign_change_base()<cr>", "Change Gitsigns Base" },
                B = { "<cmd>call v:lua.gitsign_bchange_base()<cr>", "Change Gitsigns Base" },
            },
        },
        p = {
            name = "Preview",

            a = { "<cmd>lua require'gitsigns'.blame_line({full=true})<CR>", "Blame Line" },
            p = { "<Cmd>lua vim.lsp.buf.hover({ focusable = false})<CR>", "Documentation" },
            s = { "<cmd>lua vim.lsp.buf.signature_help({ focusable = false})<CR>", "Signature" },
            d = { "<cmd>lua PeekDefinition()<CR>", "Definition" },
            E = { "<cmd>call v:lua.toggle_diagnostics()<cr>", "Toggle Diagnostics Shown" },
            e = {
                "<cmd>lua vim.diagnostic.open_float(0, {border='single', scope='line', source='always'})<CR>",
                "Diagnostics",
            },
            W = { "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", "Workspace Directory" },
            g = { "<cmd>lua require'gitsigns'.preview_hunk()<CR>", "Hunk Preview" },
            w = { "<cmd>MatchupWhereAmI??<cr>", "Preview Location" },
        },
        v = {
            name = "View",
            v = { "v:lua.commandRepeat('<leader>v', 'panelRepeat')", "Repeat Panel", expr = true, noremap = false },
            e = {
                "<cmd>let g:panelRepeat='e'<cr><cmd>TroubleToggle lsp_workspace_diagnostics<CR>",
                "Error List (Workspace)",
            },
            E = { "<cmd>let g:panelRepeat='E'<cr><cmd>TroubleToggle lsp_document_diagnostics<CR>", "Error List" },
            q = { "<cmd>let g:panelRepeat='q'<cr><cmd>TroubleToggle quickfix<CR>", "QuickFix List" },
            n = { "<cmd>let g:panelRepeat='n'<cr><cmd>TodoTrouble<cr>", "Todo List" },
            g = { "<cmd>let g:panelRepeat='g'<cr><cmd>DiffviewOpen<CR>", "Git" },
            x = { "<cmd>let g:panelRepeat='x'<cr><cmd>NvimTreeToggle<CR>", "File Tree" },
            u = { "<cmd>let g:panelRepeat='u'<cr><cmd>UndotreeToggle<CR>", "Undo Tree" },
        },
        q = {
            name = "QuickFix List",
            l = { "<cmd>cnewer<cr>", "Newer List" },
            h = { "<cmd>colder<cr>", "Older List" },
            q = { "<cmd>let g:panelRepeat='q'<cr><cmd>TroubleToggle quickfix<CR>", "QuickFix List", noremap = false },
            n = { "<cmd>TodoQuickFix<cr><let g:panelRepeat='q'<cr><cmd>Trouble quickfix<cr>", "Populate with todo items" },
            V = {
                [["<cmd> noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj **/* <cr><cmd>let g:panelRepeat='q'<cr><cmd>Trouble quickfix<cr>"]],
                "Populate With VimGrep (file select)",
                expr = true,
            },
            v = {
                [["<cmd> noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj " . input("In what files? > ") . "<cr><cmd>let g:panelRepeat='q'<cr><cmd>Trouble quickfix<cr>"]],
                "Populate With VimGrep",
                expr = true,
            },
        },
        e = {
            name = "Errors",
            p = { "<cmd>call v:lua.toggle_diagnostics()<cr>", "Toggle Diagnostics Shown" },
            r = { "<cmd>TroubleRefresh<cr>", "Refresh Errors" },
            e = { "<cmd>let g:panelRepeat='e'<cr><cmd>TroubleToggle lsp_workspace_diagnostics<CR>", "Error List" },
            E = { "<cmd>let g:panelRepeat='E'<cr><cmd>TroubleToggle lsp_document_diagnostics<CR>", "Error List (buffer)" },
            n = { "<cmd>let g:panelRepeat='n'<cr><cmd>TodoTrouble<cr>", "Todo List" },
        },
        t = {
            name = "Terminal",
            t = { "<cmd>silent !kittyPersistent normterm<cr>", "Normal Terminal" },
            i = { [[<cmd>silent exe "!kittyrepl replterm " . b:replCommand<cr>]], "REPL Terminal" },
            d = { [[<cmd>silent exe "!kittyPersistent debugterm " . b:debugCommand<cr>]], "Debug Terminal" },
            m = { "<cmd>silent !kittyPersistent maketerm<cr>", "Building Terminal" },
        },
        m = {
            name = "Make Things",
        },
        u = {
            name = "Unit Tests",
        },
        d = {
            name = "Debugging",
        },
        r = {
            name = "Refactor",
            r = { "<cmd>lua vim.lsp.buf.rename()<CR>", "Rename (LSP)" },
            f = {
                "zib<cmd>lua require('refactoring').refactor('Extract Function')<cr>",
                "Extract Function",
                noremap = false,
            },
            F = {
                "zib<cmd>lua require('refactoring').refactor('Extract Function to File')<cr>",
                "Extract Function",
                noremap = false,
            },
            v = {
                "zi,<cmd>lua require('refactoring').refactor('Extract Variable')<cr>",
                "Extract Variable",
                noremap = false,
            },
            i = {
                "zi,<cmd>lua require('refactoring').refactor('Inline Variable')<cr>",
                "Inline Variable",
                noremap = false,
            },
        },
        z = { "<cmd>ZenMode<cr>", "Zen Mode" },
        w = {
            name = "Window Managment",
            ["<leader>"] = { "<c-w>p", "Jump To Last Split" },
            o = { "<c-w>o", "Clean Up Windows" },
            O = { "<cmd>BDelete hidden<cr>", "Close All Hidden Buffers" },
            f = { "<c-w>w", "Focus Floating Window" },
            d = { "<cmd>bdelete<cr>", "Delete the current buffer" },
            D = { "<cmd>tabclose<cr>", "Close the Current Tab" },
            ["<bs>"] = { "<c-w>c", "Close Window" },
            ["<cr>"] = { "<c-w>v", "Open Window" },
            x = { "<c-w>s", "Horizontal Split" },
            v = { "<c-w>v", "Vertical Split" },
            n = { "<C-W>n", "Open New Window" },
            ["="] = { "<c-w>=", "Equal Size" },
            h = { "<c-w>H", "Move Far Left" },
            j = { "<c-w>J", "Move Far Down" },
            k = { "<c-w>K", "Move Far Up" },
            l = { "<c-w>L", "Move Far Right" },
            c = { "<c-w>c", "Close Window" },
            q = { "<c-w>c", "Close Window" },
        },
        [","] = {
            name = "Settings",
            [","] = { "<cmd>Telescope vim_options<cr>", "Vim Options" },
            s = { "<cmd>set spell!<cr>", "Toggle Spelling" },
            k = { "<cmd>Telescope keymaps<cr>", "Keymaps" },
            C = { "<cmd>Telescope colorscheme<cr>", "Color Schemes" },
            c = { "<cmd>Telescope highlights<cr>", "Highlight Groups" },
            a = { "<cmd>Telescope autocommands<cr>", "AutoCommands" },
            f = { "<cmd>Telescope filetypes<cr>", "FileTypes" },
            h = { "<cmd>Telescope help_tags<cr>", "Help Tags" },
            m = { "<cmd>Telescope man_pages<cr>", "Man Pages" },
            d = { "<cmd>call v:lua.toggle_diagnostics()<cr>", "Toggle Diagnostics Shown" },
            w = { "<cmd>set wrap!<cr>", "Toggle Text Wraping" },
            g = {
                name = "Git Settings",
                b = { "<cmd>call v:lua.gitsign_change_base()<cr>", "Change Gitsigns Base" },
                B = { "<cmd>call v:lua.gitsign_bchange_base()<cr>", "Change Gitsigns Base" },
            },
            ["/"] = { "<cmd>let @/=''<cr>", "Clear Search" },
        },
        k = {
            [[":silent !kitty @ launch --type=tab --tab-title 'Kak %:t' kak %:p +" . line(".") . ":" . col(".") . "<cr>"]],
            "Open file in Kak",
            expr = true,
        },
    },

    ["["] = {
        name = "Backward Leader",
        ["["] = { "<cmd>let g:dirJumps='s'<cr>m`<cmd>TSTextobjectGotoPreviousStart @function.outer<cr>zz", "Scope" },
        ["]"] = { "<cmd>let g:dirJumps='S'<cr>m`<cmd>TSTextobjectGotoPreviousEnd @function.outer<cr>zz", "Scope" },
        h = {
            [[&diff ? "[czz<cmd>let g:dirJumps='h'<cr>m`" : "<cmd>lua require'gitsigns'.prev_hunk()<cr>zz<cmd>let g:dirJumps='h'<cr>m`"]],
            "Hunk",
            expr = true,
        },
        Q = {
            "<cmd>let g:dirJumps='Q'<cr>m`<cmd>try <bar> cpfile <bar> catch /E553/ <bar> clast <bar> endtry<cr>zz",
            "Location Entry",
        },
        q = {
            "<cmd>let g:dirJumps='q'<cr>m`<cmd>try <bar> cprevious <bar> catch /E553/ <bar> clast <bar> endtry<cr>zz",
            "QuickFix Entry",
        },
        s = { "<cmd>let g:dirJumps='s'<cr>m`<cmd>TSTextobjectGotoPreviousStart @function.outer<cr>zz", "Scope" },
        S = { "<cmd>let g:dirJumps='S'<cr>m`<cmd>TSTextobjectGotoPrevioutEnd @function.outer<cr>zz", "Scope" },
        o = { "<cmd>let g:dirJumps='o'<cr>m`<cmd>TSTextobjectGotoPreviousStart @class.outer<cr>zz", "Class" },
        f = { "<cmd>let g:dirJumps='f'<cr>m`<cmd>TSTextobjectGotoPreviousStart @function.outer<cr>zz", "Function" },
        [","] = { "<cmd>let g:dirJumps=','<cr>m`<cmd>TSTextobjectGotoPreviousStart @parameter.inner<cr>zz", "Parameter" },
        c = { "<cmd>let g:dirJumps='c'<cr>m`<cmd>TSTextobjectGotoPreviousStart @conditional.inner<cr>zz", "Conditional" },
        C = { "<cmd>let g:dirJumps='C'<cr>m`<cmd>TSTextobjectGotoPreviousStart @comment.outer<cr>zz", "Comment" },
        b = { "<cmd>let g:dirJumps='b'<cr>m`<cmd>TSTextobjectGotoPreviousStart @block.outer<cr>zz", "Block" },
        O = { "<cmd>let g:dirJumps='O'<cr>m`<cmd>TSTextobjectGotoPreviousEnd @class.outer<cr>zz", "Class" },
        F = { "<cmd>let g:dirJumps='F'<cr>m`<cmd>TSTextobjectGotoPreviousEnd @function.outer<cr>zz", "Function" },
        ["<"] = { "<cmd>let g:dirJumps='<'<cr>m`<cmd>TSTextobjectGotoPreviousEnd @parameter.inner<cr>zz", "Parameter" },
        B = { "<cmd>let g:dirJumps='B'<cr>m`<cmd>TSTextobjectGotoPreviousEnd @block.outer<cr>zz", "Block" },
        n = { "<cmd>let g:dirJumps='search'<cr>m`Nzz", "Search Result" },
        e = {
            "<cmd>lua vim.diagnostic.goto_prev({ float = {border='single', scope='cursor', source='always'}})<CR>zz<cmd>let g:dirJumps='e'<cr>m`",
            "Diagnostics",
        },
        p = { "<cmd>let g:dirJumps='p'<cr>{zz", "Paragraph" },
        E = {
            a = {
                "<cmd>lua vim.diagnostic.goto_prev({ severity='Error', float = {border='single', scope='cursor', source='always'}})<CR>zz<cmd>let g:dirJumps='Ea'<cr>m`",
                "Error",
            },
            r = {
                "<cmd>lua vim.diagnostic.goto_prev({ severity='Warn', float = {border='single', scope='cursor', source='always'}})<CR>zz<cmd>let g:dirJumps='Er'<cr>m`",
                "Warn",
            },
            s = {
                "<cmd>lua vim.diagnostic.goto_prev({ severity='Info', float = {border='single', scope='cursor', source='always'}})<CR>zz<cmd>let g:dirJumps='Es'<cr>m`",
                "Info",
            },
            t = {
                "<cmd>lua vim.diagnostic.goto_prev({ severity='Hint', float = {border='single', scope='cursor', source='always'}})<CR>zz<cmd>let g:dirJumps='Et'<cr>m`",
                "Hint",
            },
        },
    },

    ["]"] = {
        name = "Forward Leader",
        ["]"] = { "<cmd>let g:dirJumps='s'<cr>m`<cmd>TSTextobjectGotoNextStart @function.outer<cr>zz", "Scope" },
        ["["] = { "<cmd>let g:dirJumps='S'<cr>m`<cmd>TSTextobjectGotoNextEnd @function.outer<cr>zz", "Scope" },
        h = {
            [[&diff ? "]czz<cmd>let g:dirJumps='h'<cr>m`" : "<cmd>lua require'gitsigns'.next_hunk()<cr>zz<cmd>let g:dirJumps='h'<cr>m`"]],
            "Hunk",
            expr = true,
        },
        Q = {
            "<cmd>let g:dirJumps='Q'<cr>m`<cmd>try <bar> cnfile <bar> catch /E553/ <bar> cfirst <bar> endtry<cr>zz",
            "Location Entry",
        },
        q = {
            "<cmd>let g:dirJumps='q'<cr>m`<cmd>try <bar> cnext <bar> catch /E553/ <bar> cfirst <bar> endtry<cr>zz",
            "QuickFix Entry",
        },
        s = { "<cmd>let g:dirJumps='s'<cr>m`<cmd>TSTextobjectGotoNextStart @function.outer<cr>zz", "Scope" },
        S = { "<cmd>let g:dirJumps='S'<cr>m`<cmd>TSTextobjectGotoNextEnd @function.outer<cr>zz", "Scope" },
        d = { "<cmd>let g:dirJumps='D'<cr>m`<cmd>TSTextobjectGotoNextStart @comment.outer<cr>zz", "Comment" },
        o = { "<cmd>let g:dirJumps='o'<cr>m`<cmd>TSTextobjectGotoNextStart @class.outer<cr>zz", "Class" },
        f = { "<cmd>let g:dirJumps='f'<cr>m`<cmd>TSTextobjectGotoNextStart @function.outer<cr>zz", "Function" },
        c = { "<cmd>let g:dirJumps='c'<cr>m`<cmd>TSTextobjectGotoNextStart @conditional.inner<cr>zz", "Conditional" },
        [","] = { "<cmd>let g:dirJumps=','<cr>m`<cmd>TSTextobjectGotoNextStart @parameter.inner<cr>zz", "Parameter" },
        b = { "<cmd>let g:dirJumps='b'<cr>m`<cmd>TSTextobjectGotoNextStart @block.outer<cr>zz", "Block" },
        O = { "<cmd>let g:dirJumps='O'<cr>m`<cmd>TSTextobjectGotoNextEnd @class.outer<cr>zz", "Class (end)" },
        F = { "<cmd>let g:dirJumps='F'<cr>m`<cmd>TSTextobjectGotoNextEnd @function.outer<cr>zz", "Function (end)" },
        C = { "<cmd>let g:dirJumps='C'<cr>m`<cmd>TSTextobjectGotoNextEnd @conditional.inner<cr>zz", "Conditional (end)" },
        ["<"] = { "<cmd>let g:dirJumps='<'<cr>m`<cmd>TSTextobjectGotoNextEnd @parameter.inner<cr>zz", "Parameter (end)" },
        B = { "<cmd>let g:dirJumps='B'<cr>m`<cmd>TSTextobjectGotoNextEnd @block.outer<cr>zz", "Block (end)" },
        n = { "<cmd>let g:dirJumps='search'<cr>m`nzz", "Search Result" },
        e = {
            "<cmd>lua vim.diagnostic.goto_next({ float = {border='single', scope='cursor', source='always'}})<CR>zz<cmd>let g:dirJumps='e'<cr>m`",
            "Diagnostics",
        },
        p = { "<cmd>let g:dirJumps='p'<cr>}zz", "Paragraph" },
        E = {
            a = {
                "<cmd>lua vim.diagnostic.goto_next({ severity='Error', float = {border='single', scope='cursor', source='always'}})<CR>zz<cmd>let g:dirJumps='Ea'<cr>m`",
                "Error",
            },
            r = {
                "<cmd>lua vim.diagnostic.goto_next({ severity='Warn', float = {border='single', scope='cursor', source='always'}})<CR>zz<cmd>let g:dirJumps='Er'<cr>m`",
                "Warn",
            },
            s = {
                "<cmd>lua vim.diagnostic.goto_next({ severity='Info', float = {border='single', scope='cursor', source='always'}})<CR>zz<cmd>let g:dirJumps='Es'<cr>m`",
                "Info",
            },
            t = {
                "<cmd>lua vim.diagnostic.goto_next({ severity='Hint', float = {border='single', scope='cursor', source='always'}})<CR>zz<cmd>let g:dirJumps='Et'<cr>m`",
                "Hint",
            },
        },
    },

    ["<localleader>"] = {
        name = "Local Leader",
    },
})

-- Git Diff Bindings
if vim.api.nvim_win_get_option(0, "diff") then
    require("which-key").register({
        ["<leader>"] = {
            ["["] = { "<cmd>diffget LOCAL<cr>", "Take From Local Change" },
            ["]"] = { "<cmd>diffget REMOTE<cr>", "Take From Remote Change" },
            ["<leader>"] = { "<cmd>diffget BASE<cr>", "Take From Base" },
        },
    })
end

-- Visual Bindings
require("which-key").register({
    ["g,"] = {
        name = "User Commands",

        [";"] = { "q:", "Command Buffer" },

        j = { "J", "Join" },
        k = { "c<cr><esc>", "Split" },

        r = { "<plug>(SubversiveSubstitute)", "Substitute" },
        -- t = { "<Plug>(EasyAlign)", "Align" },
        c = { "Comment" },
        -- h = { "gv<cmd>lua require'surround'.surround_add()<cr>", "Hug" },

        f = {
            name = "Formatting",

            f = { "<cmd>lua vim.lsp.buf.range_formatting()<CR>", "Format" },
            ["<space>"] = { [[:%s/\v[^^ ]\zs  / /g<cr>]], "Remove Double Spaces" },
            w = { [["!par w" . &textwidth . "<cr>"]], "Wrap to Textwidth", expr = true },

            l = { ":left<cr>", "Left Allign" },
            c = { ":center<cr>", "Centre Allign" },
            r = { ":right<cr>", "Right Allign" },
        },

        s = {
            name = "Change Case",

            p = { "<Plug>CaserVMixedCase", "Pascal Case" },
            c = { "<Plug>CaserVCamelCase", "Camel Case" },
            ["_"] = { "<Plug>CaserVSnakeCase", "Snake Case" },
            u = { "<Plug>CaserVUpperCase", "Upper Case" },
            t = { "<Plug>CaserVTitleCase", "Title Case" },
            s = { "<Plug>CaserVSentenceCase", "Sentance Case" },
            ["<space>"] = { "<Plug>CaserVSpaceCase", "Space Case" },
            ["-"] = { "<Plug>CaserVKebabCase", "Kebab Case" },
            k = { "<Plug>CaserVTitleKebabCase", "Title Case" },
            ["."] = { "<Plug>CaserVDotCase", "Dot Case" },
        },
    },
    ["<leader>"] = {
        ["."] = { "<cmd>Telescope lsp_range_code_actions theme=get_cursor<CR>", "Code Actions" },
        r = {
            name = "Refactor",
            V = { "<plug>(ExtractVarVis)", "Extract Variable" },
            f = { "<cmd>lua require('refactoring').refactor('Extract Function')<cr>", "Extract Function" },
            F = { "<cmd>lua require('refactoring').refactor('Extract Function to File')<cr>", "Extract Function" },
            v = { "<cmd>lua require('refactoring').refactor('Extract Variable')<cr>", "Extract Variable" },
            i = { "<cmd>lua require('refactoring').refactor('Inline Variable')<cr>", "Inline Variable" },
        },
        g = {
            s = { "<cmd>lua require'gitsigns'.stage_hunk({vim.fn.line('.'), vim.fn.line('.')})", "Stage Hunks in Range" },
            r = { "<cmd>lua require'gitsigns'.reset_hunk({vim.fn.line('.'), vim.fn.line('.')})", "Reset Hunks in Range" },
        },
        z = {},
    },
    a = {
        name = "around",
        h = { ":<c-u>Gitsigns selct_hunk<cr>", "Git Hunk" },
        B = { ":<c-u>TSTextobjectSelect @block.outer<cr>", "Block" },
        c = { ":<c-u>TSTextobjectSelect @conditional.outer<cr>", "Conditional" },
        [","] = { ":<c-u>TSTextobjectSelect @parameter.outer<cr>", "Parameter" },
        d = { ":<c-u>TSTextobjectSelect @comment.outer<cr>", "Comment" },
        f = { ":<c-u>TSTextobjectSelect @function.outer<cr>", "Function" },
        F = { ":<c-u>TSTextobjectSelect @call.outer<cr>", "Function" },
        o = { ":<c-u>TSTextobjectSelect @class.outer<cr>", "Class" },
        v = { "<cmd>normal! H^oL$<cr>", "Select Viewport" },
        n = {
            name = "Next",
            s = { ":<c-u>call v:lua.mapped_targets(v:count, ')', 'as')<cr>", "Sentance" },
            w = { ":<c-u>call v:lua.mapped_targets(v:count, 'w', 'aw')<cr>", "Word" },
            W = { ":<c-u>call v:lua.mapped_targets(v:count, 'W', 'aW')<cr>", "WORD" },
            ["$w"] = { ":<c-u>call v:lua.mapped_targets(v:count, '$w', 'a$w')<cr>", "Sub Word" },
            p = { ":<c-u>call v:lua.paragraph_targets(v:count, 1)<cr>", "Paragraph" },
            h = { ":<c-u>call v:lua.git_target(v:count, 'true')<cr>", "Git Hunk" },
            B = { ":<c-u>call v:lua.ts_target(v:count, '@block.outer')<cr>", "Block" },
            c = { ":<c-u>call v:lua.ts_target(v:count, '@conditional.outer')<cr>", "Conditional" },
            [","] = { ":<c-u>call v:lua.ts_target(v:count, '@parameter.outer')<cr>", "Parameter" },
            d = { ":<c-u>call v:lua.ts_target(v:count, '@comment.outer')<cr>", "Comment" },
            f = { ":<c-u>call v:lua.ts_target(v:count, '@function.outer')<cr>", "Function" },
            F = { ":<c-u>call v:lua.ts_target(v:count, '@call.outer')<cr>", "Function" },
            l = { ":<c-u>call v:lua.ts_target(v:count, '@loop.outer')<cr>", "Loop" },
            o = { ":<c-u>call v:lua.ts_target(v:count, '@class.outer')<cr>", "Class" },
        },
        l = {
            name = "Previous",
            s = { ":<c-u>call v:lua.mapped_targets_back(v:count, '(', 'g(', 'as')<cr>", "Sentance" },
            w = { ":<c-u>call v:lua.mapped_targets_back(v:count, 'b', 'ge', 'aw')<cr>", "Word" },
            W = { ":<c-u>call v:lua.mapped_targets_back(v:count, 'B', 'gE', 'aW')<cr>", "WORD" },
            ["$w"] = { ":<c-u>call v:lua.mapped_targets_back(v:count, '$ge', '$b', 'a$w')<cr>", "Sub Word" },
            p = { ":<c-u>call v:lua.paragraph_targets_back(v:count, 1)<cr>", "Paragraph" },
            h = { ":<c-u>call v:lua.git_target(v:count, 'false')<cr>", "Git Hunk" },
            B = { ":<c-u>call v:lua.ts_target_back(v:count, '@block.outer')<cr>", "Block" },
            c = { ":<c-u>call v:lua.ts_target_back(v:count, '@conditional.outer')<cr>", "Conditional" },
            [","] = { ":<c-u>call v:lua.ts_target_back(v:count, '@parameter.outer')<cr>", "Parameter" },
            d = { ":<c-u>call v:lua.ts_target_back(v:count, '@comment.outer')<cr>", "Comment" },
            f = { ":<c-u>call v:lua.ts_target_back(v:count, '@function.outer')<cr>", "Function" },
            F = { ":<c-u>call v:lua.ts_target_back(v:count, '@call.outer')<cr>", "Function" },
            l = { ":<c-u>call v:lua.ts_target_back(v:count, '@loop.outer')<cr>", "Loop" },
            o = { ":<c-u>call v:lua.ts_target_back(v:count, '@class.outer')<cr>", "Class" },
        },
    },
    i = {
        name = "inside",
        h = { ":<c-u>Gitsigns selct_hunk<cr>", "Git Hunk" },
        B = { ":<c-u>TSTextobjectSelect @block.inner<cr>", "Block" },
        c = { ":<c-u>TSTextobjectSelect @conditional.inner<cr>", "Conditional" },
        [","] = { ":<c-u>TSTextobjectSelect @parameter.inner<cr>", "Parameter" },
        d = { ":<c-u>TSTextobjectSelect @comment.outer<cr>", "Comment" },
        f = { ":<c-u>TSTextobjectSelect @function.inner<cr>", "Function" },
        F = { ":<c-u>TSTextobjectSelect @call.inner<cr>", "Function" },
        o = { ":<c-u>TSTextobjectSelect @class.inner<cr>", "Class" },
        v = { "<cmd>normal! H^oL$<cr>", "Select Viewport" },
        n = {
            name = "Next",
            s = { ":<c-u>call v:lua.mapped_targets(v:count, ')', 'is')<cr>", "Sentance" },
            w = { ":<c-u>call v:lua.mapped_targets(v:count, 'w', 'iw')<cr>", "Word" },
            W = { ":<c-u>call v:lua.mapped_targets(v:count, 'W', 'iW')<cr>", "WORD" },
            ["$w"] = { ":<c-u>call v:lua.mapped_targets(v:count, '$w', 'i$w')<cr>", "Sub Word" },
            p = { ":<c-u>call v:lua.paragraph_targets(v:count, 0)<cr>", "Paragraph" },
            h = { ":<c-u>call v:lua.git_target(v:count, 'true')<cr>", "Git Hunk" },
            B = { ":<c-u>call v:lua.ts_target(v:count, '@block.inner')<cr>", "Block" },
            c = { ":<c-u>call v:lua.ts_target(v:count, '@conditional.inner')<cr>", "Conditional" },
            [","] = { ":<c-u>call v:lua.ts_target(v:count, '@parameter.inner')<cr>", "Parameter" },
            d = { ":<c-u>call v:lua.ts_target(v:count, '@comment.outer')<cr>", "Comment" },
            f = { ":<c-u>call v:lua.ts_target(v:count, '@function.inner')<cr>", "Function" },
            F = { ":<c-u>call v:lua.ts_target(v:count, '@call.inner')<cr>", "Function" },
            l = { ":<c-u>call v:lua.ts_target(v:count, '@loop.inner')<cr>", "Loop" },
            o = { ":<c-u>call v:lua.ts_target(v:count, '@class.inner')<cr>", "Class" },
        },
        l = {
            name = "Previous",
            s = { ":<c-u>call v:lua.mapped_targets_back(v:count, '(', 'g(', 'is')<cr>", "Sentance" },
            w = { ":<c-u>call v:lua.mapped_targets_back(v:count, 'b', 'ge', 'iw')<cr>", "Word" },
            W = { ":<c-u>call v:lua.mapped_targets_back(v:count, 'B', 'gE', 'iW')<cr>", "WORD" },
            ["$w"] = { ":<c-u>call v:lua.mapped_targets_back(v:count, '$ge', '$b', 'i$w')<cr>", "Sub Word" },
            p = { ":<c-u>call v:lua.paragraph_targets_back(v:count, 0)<cr>", "Paragraph" },
            h = { ":<c-u>call v:lua.git_target(v:count, 'false')<cr>", "Git Hunk" },
            B = { ":<c-u>call v:lua.ts_target_back(v:count, '@block.inner')<cr>", "Block" },
            c = { ":<c-u>call v:lua.ts_target_back(v:count, '@conditional.inner')<cr>", "Conditional" },
            [","] = { ":<c-u>call v:lua.ts_target_back(v:count, '@parameter.inner')<cr>", "Parameter" },
            d = { ":<c-u>call v:lua.ts_target_back(v:count, '@comment.outer')<cr>", "Comment" },
            f = { ":<c-u>call v:lua.ts_target_back(v:count, '@function.inner')<cr>", "Function" },
            F = { ":<c-u>call v:lua.ts_target_back(v:count, '@call.inner')<cr>", "Function" },
            l = { ":<c-u>call v:lua.ts_target_back(v:count, '@loop.inner')<cr>", "Loop" },
            o = { ":<c-u>call v:lua.ts_target_back(v:count, '@class.inner')<cr>", "Class" },
        },
    },
    ["["] = {
        name = "Backward Leader",
        ["["] = { "v:lua.commandRepeat('[', 'dirJumps')", "Repeat Last", expr = true, noremap = false },
        h = {
            [[&diff ? "[czz<cmd>let g:dirJumps='h'<cr>m`" : "<cmd>lua require'gitsigns'.prev_hunk()<cr>zz<cmd>let g:dirJumps='h'<cr>m`"]],
            "Hunk",
            expr = true,
        },
        Q = {
            "<cmd>let g:dirJumps='Q'<cr>m`<cmd>try <bar> cpfile <bar> catch /E553/ <bar> clast <bar> endtry<cr>zz",
            "Location Entry",
        },
        q = {
            "<cmd>let g:dirJumps='q'<cr>m`<cmd>try <bar> cprevious <bar> catch /E553/ <bar> clast <bar> endtry<cr>zz",
            "QuickFix Entry",
        },
        s = { "<cmd>let g:dirJumps='s'<cr>m`[szz", "Spelling Mistake" },
        ["]"] = { "<cmd>let g:dirJumps=']'<cr>m`[]zz", "Section End", noremap = false },
        ["{"] = { "<cmd>let g:dirJumps='{'<cr>m`[[zz", "Section Start" },
        ["}"] = { "<cmd>let g:dirJumps='{'<cr>m`[[zz", "Section Start" },
        -- ["*"] = { "<cmd>let g:dirJumps='*'<cr>m`[#zz", "Function Call", noremap = false },
        o = { "<cmd>let g:dirJumps='o'<cr>m`<cmd>TSTextobjectGotoPreviousStart @class.outer<cr>zz", "Class" },
        f = { "<cmd>let g:dirJumps='f'<cr>m`<cmd>TSTextobjectGotoPreviousStart @function.outer<cr>zz", "Function" },
        [","] = { "<cmd>let g:dirJumps=','<cr>m`<cmd>TSTextobjectGotoPreviousStart @parameter.inner<cr>zz", "Parameter" },
        c = { "<cmd>let g:dirJumps='c'<cr>m`<cmd>TSTextobjectGotoPreviousStart @conditional.inner<cr>zz", "Conditional" },
        C = { "<cmd>let g:dirJumps='C'<cr>m`<cmd>TSTextobjectGotoPreviousStart @comment.outer<cr>zz", "Comment" },
        l = { "<cmd>let g:dirJumps='l'<cr>m`<cmd>TSTextobjectGotoPreviousStart @loop.outer<cr>zz", "Loop" },
        b = { "<cmd>let g:dirJumps='b'<cr>m`<cmd>TSTextobjectGotoPreviousStart @block.outer<cr>zz", "Block" },
        O = { "<cmd>let g:dirJumps='O'<cr>m`<cmd>TSTextobjectGotoPreviousEnd @class.outer<cr>zz", "Class" },
        F = { "<cmd>let g:dirJumps='F'<cr>m`<cmd>TSTextobjectGotoPreviousEnd @function.outer<cr>zz", "Function" },
        ["<"] = { "<cmd>let g:dirJumps='<'<cr>m`<cmd>TSTextobjectGotoPreviousEnd @parameter.inner<cr>zz", "Parameter" },
        L = { "<cmd>let g:dirJumps='L'<cr>m`<cmd>TSTextobjectGotoPreviousEnd @loop.outer<cr>zz", "Loop" },
        B = { "<cmd>let g:dirJumps='B'<cr>m`<cmd>TSTextobjectGotoPreviousEnd @block.outer<cr>zz", "Block" },
        E = {
            "<cmd>let g:dirJumps='E'<cr>m`<cmd>lua require'trouble'.previous({skip_groups=false, jump=true})<cr>zz",
            "Trouble Item",
        },
        m = { "<cmd>let g:dirJumps='m'<cr>m`[`zz", "File Marks" },
        e = {
            "<cmd>lua vim.lsp.diagnostic.goto_prev({ focusable = false , popup_opts = { border = 'single' }})<CR>zz<cmd>let g:dirJumps='e'<cr>m`",
            "Error",
        },
    },
    ["]"] = {
        name = "Forward Leader",
        ["]"] = { "v:lua.commandRepeat(']', 'dirJumps')", "Repeat Last", expr = true, noremap = false },
        h = {
            [[&diff ? "]czz<cmd>let g:dirJumps='h'<cr>m`" : "<cmd>lua require'gitsigns'.next_hunk()<cr>zz<cmd>let g:dirJumps='h'<cr>m`"]],
            "Hunk",
            expr = true,
        },
        Q = {
            "<cmd>let g:dirJumps='Q'<cr>m`<cmd>try <bar> cnfile <bar> catch /E553/ <bar> cfirst <bar> endtry<cr>zz",
            "Location Entry",
        },
        q = {
            "<cmd>let g:dirJumps='q'<cr>m`<cmd>try <bar> cnext <bar> catch /E553/ <bar> cfirst <bar> endtry<cr>zz",
            "QuickFix Entry",
        },
        s = { "<cmd>let g:dirJumps='s'<cr>m`]szz", "Spelling Mistake" },
        ["["] = { "<cmd>let g:dirJumps='['<cr>m`][zz", "Section End", noremap = false },
        ["}"] = { "<cmd>let g:dirJumps='}'<cr>m`]]zz", "Section Start", noremap = true },
        ["{"] = { "<cmd>let g:dirJumps='}'<cr>m`]]zz", "Section Start", noremap = true },
        -- ["*"] = { "<cmd>let g:dirJumps='*'<cr>m`]#zz", "Function Call", noremap = false },
        d = { "<cmd>let g:dirJumps='D'<cr>m`<cmd>TSTextobjectGotoNextStart @comment.outer<cr>zz", "Comment" },
        o = { "<cmd>let g:dirJumps='o'<cr>m`<cmd>TSTextobjectGotoNextStart @class.outer<cr>zz", "Class" },
        f = { "<cmd>let g:dirJumps='f'<cr>m`<cmd>TSTextobjectGotoNextStart @function.outer<cr>zz", "Function" },
        c = { "<cmd>let g:dirJumps='c'<cr>m`<cmd>TSTextobjectGotoNextStart @conditional.inner<cr>zz", "Conditional" },
        [","] = { "<cmd>let g:dirJumps=','<cr>m`<cmd>TSTextobjectGotoNextStart @parameter.inner<cr>zz", "Parameter" },
        l = { "<cmd>let g:dirJumps='l'<cr>m`<cmd>TSTextobjectGotoNextStart @loop.outer<cr>zz", "Loop" },
        b = { "<cmd>let g:dirJumps='b'<cr>m`<cmd>TSTextobjectGotoNextStart @block.outer<cr>zz", "Block" },
        O = { "<cmd>let g:dirJumps='O'<cr>m`<cmd>TSTextobjectGotoNextEnd @class.outer<cr>zz", "Class (end)" },
        F = { "<cmd>let g:dirJumps='F'<cr>m`<cmd>TSTextobjectGotoNextEnd @function.outer<cr>zz", "Function (end)" },
        C = { "<cmd>let g:dirJumps='C'<cr>m`<cmd>TSTextobjectGotoNextEnd @conditional.inner<cr>zz", "Conditional (end)" },
        ["<"] = { "<cmd>let g:dirJumps='<'<cr>m`<cmd>TSTextobjectGotoNextEnd @parameter.inner<cr>zz", "Parameter (end)" },
        L = { "<cmd>let g:dirJumps='L'<cr>m`<cmd>TSTextobjectGotoNextEnd @loop.outer<cr>zz", "Loop (end)" },
        B = { "<cmd>let g:dirJumps='B'<cr>m`<cmd>TSTextobjectGotoNextEnd @block.outer<cr>zz", "Block (end)" },
        E = {
            "<cmd>let g:dirJumps='E'<cr>m`<cmd>lua require'trouble'.next({skip_groups=false, jump=true})<cr>zz",
            "Trouble Item",
        },
        m = { "<cmd>let g:dirJumps='m'<cr>m`]`zz", "File Marks" },
        e = {
            "<cmd>lua vim.lsp.diagnostic.goto_next({ focusable = false , popup_opts = { border = 'single' }})<CR>zz<cmd>let g:dirJumps='e'<cr>m`",
            "Error",
        },
    },
}, {
    mode = "x",
})

require("which-key").register({
    a = {
        name = "around",
        h = { ":<c-u>Gitsigns selct_hunk<cr>", "Git Hunk" },
        B = { ":<c-u>TSTextobjectSelect @block.outer<cr>", "Block" },
        c = { ":<c-u>TSTextobjectSelect @conditional.outer<cr>", "Conditional" },
        [","] = { ":<c-u>TSTextobjectSelect @parameter.outer<cr>", "Parameter" },
        d = { ":<c-u>TSTextobjectSelect @comment.outer<cr>", "Comment" },
        f = { ":<c-u>TSTextobjectSelect @function.outer<cr>", "Function" },
        F = { ":<c-u>TSTextobjectSelect @call.outer<cr>", "Function" },
        o = { ":<c-u>TSTextobjectSelect @class.outer<cr>", "Class" },
        v = { "<cmd>exec 'normal! HVL'<cr>", "Select Viewport" },
        n = {
            name = "Next",
            s = { ":<c-u>call v:lua.mapped_targets(v:count, ')', 'as')<cr>", "Sentance" },
            w = { ":<c-u>call v:lua.mapped_targets(v:count, 'w', 'aw')<cr>", "Word" },
            W = { ":<c-u>call v:lua.mapped_targets(v:count, 'W', 'aW')<cr>", "Word" },
            ["$w"] = { ":<c-u>call v:lua.mapped_targets(v:count, '$w', 'a$w')<cr>", "Sub Word" },
            p = { ":<c-u>call v:lua.paragraph_targets(v:count, 1)<cr>", "Paragraph" },
            h = { ":<c-u>call v:lua.git_target(v:count, 'true')<cr>", "Git Hunk" },
            B = { ":<c-u>call v:lua.ts_target(v:count, '@block.outer')<cr>", "Block" },
            c = { ":<c-u>call v:lua.ts_target(v:count, '@conditional.outer')<cr>", "Conditional" },
            [","] = { ":<c-u>call v:lua.ts_target(v:count, '@parameter.outer')<cr>", "Parameter" },
            d = { ":<c-u>call v:lua.ts_target(v:count, '@comment.outer')<cr>", "Comment" },
            f = { ":<c-u>call v:lua.ts_target(v:count, '@function.outer')<cr>", "Function" },
            F = { ":<c-u>call v:lua.ts_target(v:count, '@call.outer')<cr>", "Function" },
            o = { ":<c-u>call v:lua.ts_target(v:count, '@class.outer')<cr>", "Class" },
        },
        l = {
            name = "Previous",
            s = { ":<c-u>call v:lua.mapped_targets_back(v:count, '(', 'g(', 'as')<cr>", "Sentance" },
            w = { ":<c-u>call v:lua.mapped_targets_back(v:count, 'b', 'ge', 'aw')<cr>", "Word" },
            W = { ":<c-u>call v:lua.mapped_targets_back(v:count, 'B', 'gE', 'aW')<cr>", "Word" },
            ["$w"] = { ":<c-u>call v:lua.mapped_targets_back(v:count, '$ge', '$b', 'a$w')<cr>", "Sub Word" },
            p = { ":<c-u>call v:lua.paragraph_targets_back(v:count, 1)<cr>", "Paragraph" },
            h = { ":<c-u>call v:lua.git_target(v:count, 'false')<cr>", "Git Hunk" },
            B = { ":<c-u>call v:lua.ts_target_back(v:count, '@block.outer')<cr>", "Block" },
            c = { ":<c-u>call v:lua.ts_target_back(v:count, '@conditional.outer')<cr>", "Conditional" },
            [","] = { ":<c-u>call v:lua.ts_target_back(v:count, '@parameter.outer')<cr>", "Parameter" },
            d = { ":<c-u>call v:lua.ts_target_back(v:count, '@comment.outer')<cr>", "Comment" },
            f = { ":<c-u>call v:lua.ts_target_back(v:count, '@function.outer')<cr>", "Function" },
            F = { ":<c-u>call v:lua.ts_target_back(v:count, '@call.outer')<cr>", "Function" },
            o = { ":<c-u>call v:lua.ts_target_back(v:count, '@class.outer')<cr>", "Class" },
        },
    },
    i = {
        name = "inside",
        h = { ":<c-u>Gitsigns selct_hunk<cr>", "Git Hunk" },
        B = { ":<c-u>TSTextobjectSelect @block.inner<cr>", "Block" },
        c = { ":<c-u>TSTextobjectSelect @conditional.inner<cr>", "Conditional" },
        [","] = { ":<c-u>TSTextobjectSelect @parameter.inner<cr>", "Parameter" },
        d = { ":<c-u>TSTextobjectSelect @comment.outer<cr>", "Comment" },
        f = { ":<c-u>TSTextobjectSelect @function.inner<cr>", "Function" },
        F = { ":<c-u>TSTextobjectSelect @call.inner<cr>", "Function" },
        o = { ":<c-u>TSTextobjectSelect @class.inner<cr>", "Class" },
        v = { "<cmd>exec 'normal! HVL'<cr>", "Select Viewport" },
        n = {
            name = "Next",
            s = { ":<c-u>call v:lua.mapped_targets(v:count, ')', 'is')<cr>", "Sentance" },
            w = { ":<c-u>call v:lua.mapped_targets(v:count, 'w', 'iw')<cr>", "Word" },
            W = { ":<c-u>call v:lua.mapped_targets(v:count, 'W', 'iW')<cr>", "WORD" },
            ["$w"] = { ":<c-u>call v:lua.mapped_targets(v:count, '$w', 'i$w')<cr>", "Sub Word" },
            p = { ":<c-u>call v:lua.paragraph_targets(v:count, 0)<cr>", "Paragraph" },
            h = { ":<c-u>call v:lua.git_target(v:count, 'true')<cr>", "Git Hunk" },
            B = { ":<c-u>call v:lua.ts_target(v:count, '@block.inner')<cr>", "Block" },
            c = { ":<c-u>call v:lua.ts_target(v:count, '@conditional.inner')<cr>", "Conditional" },
            [","] = { ":<c-u>call v:lua.ts_target(v:count, '@parameter.inner')<cr>", "Parameter" },
            d = { ":<c-u>call v:lua.ts_target(v:count, '@comment.outer')<cr>", "Comment" },
            f = { ":<c-u>call v:lua.ts_target(v:count, '@function.inner')<cr>", "Function" },
            F = { ":<c-u>call v:lua.ts_target(v:count, '@call.inner')<cr>", "Function" },
            o = { ":<c-u>call v:lua.ts_target(v:count, '@class.inner')<cr>", "Class" },
        },
        l = {
            name = "Previous",
            s = { ":<c-u>call v:lua.mapped_targets_back(v:count, '(', 'g(', 'is')<cr>", "Sentance" },
            w = { ":<c-u>call v:lua.mapped_targets_back(v:count, 'b', 'ge', 'iw')<cr>", "Word" },
            W = { ":<c-u>call v:lua.mapped_targets_back(v:count, 'B', 'gE', 'iW')<cr>", "WORD" },
            ["$w"] = { ":<c-u>call v:lua.mapped_targets_back(v:count, '$ge', '$b', 'i$w')<cr>", "Sub Word" },
            p = { ":<c-u>call v:lua.paragraph_targets_back(v:count, 0)<cr>", "Paragraph" },
            h = { ":<c-u>call v:lua.git_target(v:count, 'false')<cr>", "Git Hunk" },
            B = { ":<c-u>call v:lua.ts_target_back(v:count, '@block.inner')<cr>", "Block" },
            c = { ":<c-u>call v:lua.ts_target_back(v:count, '@conditional.inner')<cr>", "Conditional" },
            [","] = { ":<c-u>call v:lua.ts_target_back(v:count, '@parameter.inner')<cr>", "Parameter" },
            d = { ":<c-u>call v:lua.ts_target_back(v:count, '@comment.outer')<cr>", "Comment" },
            f = { ":<c-u>call v:lua.ts_target_back(v:count, '@function.inner')<cr>", "Function" },
            F = { ":<c-u>call v:lua.ts_target_back(v:count, '@call.inner')<cr>", "Function" },
            o = { ":<c-u>call v:lua.ts_target_back(v:count, '@class.inner')<cr>", "Class" },
        },
    },
}, {
    mode = "o",
})

imap("<c-a>", "<C-O>^")
imap("<c-e>", "<END>")
cmap("<c-a>", "<HOME>")
cmap("<c-e>", "<END>")

imap("<s-tab>", "<c-d>")
smap("<s-tab>", "<c-d>")

imap("<c-]>", "<plug>luasnip-next-choice")
smap("<c-]>", "<plug>luasnip-next-choice")

imap("<c-space>", "v:lua.cmp_toggle()", "expr")
smap("<c-space>", "v:lua.cmp_toggle()", "expr")

cmap("<esc>", "v:lua.cmp_esc()", "expr")
imap("<S-Tab>", "v:lua.s_tab_complete()", "expr")
imap("<Tab>", "v:lua.tab_complete()", "expr")
smap("<S-Tab>", "v:lua.s_tab_complete()", "expr")
smap("<Tab>", "v:lua.tab_complete()", "expr")

inoremap(
    "<Down>",
    [[<cmd>lua require("cmp").select_next_item({ behavior = require("cmp").SelectBehavior.Select })<cr>]]
)
inoremap("<Up>", [[<cmd>lua require("cmp").select_prev_item({ behavior = require("cmp").SelectBehavior.Select })<cr>]])
snoremap(
    "<Down>",
    [[<cmd>lua require("cmp").select_next_item({ behavior = require("cmp").SelectBehavior.Select })<cr>]]
)
snoremap("<Up>", [[<cmd>lua require("cmp").select_prev_item({ behavior = require("cmp").SelectBehavior.Select })<cr>]])
