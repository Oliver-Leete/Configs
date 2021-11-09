----------------------------------------------------------------------------------------------------
--                      _   _   ______    ____   __      __  _____   __  __                       --
--                     | \ | | |  ____|  / __ \  \ \    / / |_   _| |  \/  |                      --
--                     |  \| | | |__    | |  | |  \ \  / /    | |   | \  / |                      --
--                     | . ` | |  __|   | |  | |   \ \/ /     | |   | |\/| |                      --
--                     | |\  | | |____  | |__| |    \  /     _| |_  | |  | |                      --
--                     |_| \_| |______|  \____/      \/     |_____| |_|  |_|                      --
--                                                                                                --
----------------------------------------------------------------------------------------------------
-- Oliver Leete <oliverleete@gmail.com>                                                            --
-- https://github.com/oliver-leete                                                                 --
----------------------------------------------------------------------------------------------------

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

-- Normal Bindings
require("which-key").register({
    j = { [[v:count?(v:count>5?"m'".v:count:'').'j':'gj']], "down", expr = true },
    k = { [[v:count?(v:count>5?"m'".v:count:'').'k':'gk']], "up", expr = true },
    H = { [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], "Start of Line", expr = true },
    L = { [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], "End of Line", expr = true },
    J = { "gi", "Goto Last Insert" },
    U = { "<c-r>", "Redo" },
    Q = { "@q", "Play The Temp Macro" },
    s = { "<cmd>lua require'hop'.hint_char1()<cr>", "Hop Char" },
    S = { "<cmd>ISwapWith<cr>", "Swap Things" },

    ["'"] = { "`", "Jump to mark location" },
    ["`"] = { "'", "Jump to mark line" },

    ["<right>"] = { "<cmd>BufferLineCycleNext<cr>", "Next Buffer" },
    ["<left>"] = { "<cmd>BufferLineCyclePrev<cr>", "Prev Buffer" },
    ["<C-right>"] = { "<cmd>tabnext<cr>", "Next Tab" },
    ["<C-left>"] = { "<cmd>tabprevious<cr>", "Prev Tab" },
    ["<S-right>"] = { "<cmd>tabnext<cr>", "Next Tab" },
    ["<S-left>"] = { "<cmd>tabprevious<cr>", "Prev Tab" },

    ["n"] = { "v:lua.commandRepeat(']', 'dirJumps')", "Repeat Last", expr = true, noremap = false },
    ["N"] = { "v:lua.commandRepeat('[', 'dirJumps')", "Repeat Last", expr = true, noremap = false },

    ["<cr>"] = { "<plug>(sendOp)", "Send to Repl" },
    ["<cr><cr>"] = { "<cmd>call v:lua.sendLines(v:count)<cr>", "Send Line to Repl" },

    g = {
        name = "Goto",

        g = { "gg", "Buffer Top" },
        j = { "G", "Buffer Bottom" },
        k = { "gg", "Buffer Top" },
        h = { "^", "Line Begining" },
        l = { "$", "Line End" },

        t = { "H", "Window Top" },
        c = { "M", "Window Center" },
        b = { "L", "Window Bottom" },

        f = { "gf", "Open File" },
        F = { "<cmd>vsplit<cr>gf", "Open File in Split" },

        d = { "<cmd>lua require('telescope.builtin').lsp_definitions()<cr>", "Definitions" },
        r = { "<cmd>Telescope lsp_references<cr>", "References" },
        i = { "<cmd>lua require('telescope.builtin').lsp_implementations()<CR>", "Implementations" },
        o = { "<cmd>lua require('telescope.builtin').lsp_type_definitions()<cr>", "Object Deffinition" },

        D = { "<cmd>lua require('telescope.builtin').lsp_definitions({jump_type='vsplit'})<cr>", "Definitions (split)" },
        R = { "<cmd>lua require('telescope.builtin').lep_references({jump_type='vsplit'})<cr>", "References (split)" },
        I = {
            "<cmd>lua require('telescope.builtin').lsp_implementations({jump_type='vsplit'})<CR>",
            "Implementations (split)",
        },
        O = {
            "<cmd>lua require('telescope.builtin').lsp_type_definitions({jump_type='vsplit'})<cr>",
            "Object Deffinition (split)",
        },

        p = { "<plug>(paste-away-after)", "Paste After Object" },
        P = { "<plug>(paste-away-before)", "Paste Before Object" },

        ["{"] = { "<Plug>(ninja-insert)a", "Go Insert Left Outside" },
        ["("] = { "<Plug>(ninja-insert)i", "Go Insert Left Inside" },
        [")"] = { "<Plug>(ninja-append)i", "Go Insert Right Inside" },
        ["}"] = { "<Plug>(ninja-append)a", "Go Insert Right Outside" },
    },

    G = {
        name = "Goto (Select)",

        g = { "vgg", "Buffer Top" },
        j = { "vG", "Buffer Bottom" },
        k = { "vgg", "Buffer Top" },
        h = { "v^", "Line Begining" },
        l = { "v$", "Line End" },

        t = { "vH", "Window Top" },
        c = { "vM", "Window Bottom" },
        b = { "vL", "Window Center" },
    },

    v = {
        name = "View",

        t = { "zt", "Cursor On Top" },
        v = { "zz", "Centre Cursor (Vertically)" },
        c = { "zz", "Centre Cursor (Vertically)" },
        b = { "zb", "Cursor On Bottom" },

        f = { "zs", "Cursor At First" },
        m = { "<cmd>set sidescrolloff=999<cr><cmd>set sidescrolloff=0<cr>", "Centre Cursor (Horizontally)" },
        e = { "ze", "Cursor At End" },

        h = { "zh", "Scroll Left" },
        l = { "zl", "Scroll Right" },
        j = { "<c-e>", "Scroll Down" },
        k = { "<c-y>", "Scroll Up" },

        u = { "<c-u>", "Half Page Up" },
        d = { "<c-d>", "Half Page Down" },

        s = { "<cmd>normal! HVL<cr>", "Select Viewport" },
    },

    V = {
        name = "View (lock)",

        t = { "vt<cmd>WhichKey V<cr>", "Cursor On Top", noremap = false },
        v = { "vv<cmd>WhichKey V<cr>", "Centre Cursor (Vertically)", noremap = false },
        b = { "vb<cmd>WhichKey V<cr>", "Cursor On Bottom", noremap = false },

        f = { "vf<cmd>WhichKey V<cr>", "Cursor At First", noremap = false },
        m = { "vm<cmd>WhichKey V<cr>", "Centre Cursor (Horizontally)", noremap = false },
        e = { "ve<cmd>WhichKey V<cr>", "Cursor At End", noremap = false },

        h = { "vh<cmd>WhichKey V<cr>", "Scroll Left", noremap = false },
        l = { "vl<cmd>WhichKey V<cr>", "Scroll Right", noremap = false },
        j = { "vj<cmd>WhichKey V<cr>", "Scroll Down", noremap = false },
        k = { "vk<cmd>WhichKey V<cr>", "Scroll Up", noremap = false },

        u = { "vu<cmd>WhichKey V<cr>", "Half Page Up", noremap = false },
        d = { "vd<cmd>WhichKey V<cr>", "Half Page Down", noremap = false },

        o = { "vo<cmd>WhichKey V<cr>", "Open Fold", noremap = false },
        c = { "vc<cmd>WhichKey V<cr>", "Close Fold", noremap = false },
        s = { "vs<cmd>WhichKey V<cr>", "Select Viewport", noremap = false },
    },

    z = {
        name = "Select Mode",

        h = { "<m-v>{", "Left Outside", noremap = false },
        n = { "<m-v>(", "Left Inside", noremap = false },
        e = { "<m-v>)", "Right Inside", noremap = false },
        l = { "<m-v>}", "Right Outside", noremap = false },

        ["{"] = { "<m-v>{", "Left Outside", noremap = false },
        ["("] = { "<m-v>(", "Left Inside", noremap = false },
        [")"] = { "<m-v>)", "Right Inside", noremap = false },
        ["}"] = { "<m-v>}", "Right Outside", noremap = false },

        i = { "<m-v>i", "Inside", noremap = false },
        o = { "<m-v>a", "Outside", noremap = false },
    },

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

        ["<"] = {
            name = "Swap With Previous",

            a = { "<cmd>TSTextobjectSwapPrevious @parameter.inner<cr>", "Parameter" },
            o = { "<cmd>TSTextobjectSwapPrevious @class.outer<cr>", "Class" },
            f = { "<cmd>TSTextobjectSwapPrevious @function.outer<cr>", "Function" },
            F = { "<cmd>TSTextobjectSwapPrevious @call.outer<cr>", "Call" },
            C = { "<cmd>TSTextobjectSwapPrevious @conditional.outer<cr>", "Conditional (Outer)" },
            c = { "<cmd>TSTextobjectSwapPrevious @conditional.inner<cr>", "Conditional (Inner)" },
            l = { "<cmd>TSTextobjectSwapPrevious @loop.outer<cr>", "Loop" },
            b = { "<cmd>TSTextobjectSwapPrevious @block.outer<cr>", "Block" },
        },

        [">"] = {
            name = "Swap With Next",

            a = { "<cmd>TSTextobjectSwapNext @parameter.inner<cr>", "Parameter" },
            o = { "<cmd>TSTextobjectSwapNext @class.outer<cr>", "Class" },
            f = { "<cmd>TSTextobjectSwapNext @function.outer<cr>", "Function" },
            F = { "<cmd>TSTextobjectSwapNext @call.outer<cr>", "Call" },
            C = { "<cmd>TSTextobjectSwapNext @conditional.outer<cr>", "Conditional (Outer)" },
            c = { "<cmd>TSTextobjectSwapNext @conditional.inner<cr>", "Conditional (Inner)" },
            l = { "<cmd>TSTextobjectSwapNext @loop.outer<cr>", "Loop" },
            b = { "<cmd>TSTextobjectSwapNext @block.outer<cr>", "Block" },
        },

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
