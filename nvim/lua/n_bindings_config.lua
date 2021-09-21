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
        marks = true,
        registers = true,
        spelling = {
            enabled = false,
            suggestions = 20,
        },
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
        ["g,hh"] = "Hug Around",
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
    icons = {
        breadcrumb = "»",
        separator = "➜",
        group = "+",
    },
    window = {
        border = "none",
        position = "bottom",
        margin = { 1, 0, 1, 0 },
        padding = { 2, 2, 2, 2 },
    },
    layout = {
        height = { min = 4, max = 30 },
        width = { min = 20, max = 50 },
        spacing = 2,
    },
    hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ ", "<plug>", "<Plug>" },
    show_help = true,
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

    ["<C-down>"] = { "<cmd>try<bar>cnext<bar>catch/E553/<bar>cfirst<bar>endtry<CR>", "Next Quickfix" },
    ["<C-up>"] = { "<cmd>try<bar>cprevious<bar>catch/E553/<bar>clast<bar>endtry<CR>", "Prev Quickfix" },
    ["<S-down>"] = { "<cmd>try<bar>lnext<bar>catch/E553/<bar>lfirst<bar>endtry<CR>", "Next Loclist" },
    ["<S-up>"] = { "<cmd>try<bar>lprevious<bar>catch/E553/<bar>llast<bar>endtry<CR>", "Prev Loclist" },

    g = {
        name = "Goto",

        g = { "gg", "Buffer Top" },
        j = { "G", "Buffer Bottom" },
        k = { "gg", "Buffer Top" },
        h = { "^", "Line Begining" },
        l = { "$", "Line End" },

        t = { "H", "Window Top" },
        c = { "M", "Window Bottom" },
        b = { "L", "Window Center" },

        f = { "gf", "Open File" },
        F = { "<cmd>vsplit<cr>gf", "Open File in Split" },

        d = { "<cmd>lua require('telescope.builtin').lsp_definitions({jump_type='vsplit'})<cr>", "Definitions" },
        r = { "<cmd>Telescope lsp_references<cr>", "References" },
        i = {
            "<cmd>lua require('telescope.builtin').lsp_implementations({jump_type='vsplit'})<CR>",
            "Implementations",
        },
        T = {
            "<cmd>lua require('telescope.builtin').lsp_type_definitions({jump_type='vsplit'})<cr>",
            "Type Deffinition",
        },

        p = { "<plug>(paste-away-after)", "Paste After Object" },
        P = { "<plug>(paste-away-before)", "Paste Before Object" },

        ["{"] = { "<Plug>(ninja-insert)a", "Go Insert Left Outside" },
        ["("] = { "<Plug>(ninja-insert)i", "Go Insert Left Inside" },
        [")"] = { "<Plug>(ninja-append)i", "Go Insert Right Inside" },
        ["}"] = { "<Plug>(ninja-append)a", "Go Insert Right Outside" },
    },

    v = {
        name = "View",

        t = { "zt", "Cursor On Top" },
        v = { "zz", "Centre Cursor (Vertically)" },
        b = { "zb", "Cursor On Bottom" },

        f = { "zs", "Cursor At First" },
        m = { "<cmd>set sidescrolloff=999<cr><cmd>set sidescrolloff=0<cr>", "Centre Cursor (Horizontally)" },
        e = { "ze", "Cursor At End" },

        h = { "zh", "Scroll Left" },
        l = { "zl", "Scroll Right" },
        j = { "<c-e>", "Scroll Down" },
        k = { "<c-y>", "Scroll Up" },

        o = { "za", "Open Fold" },
        c = { "zc", "Close Fold" },
        s = { "<cmd>normal! HVL<cr>", "Select Viewport" },
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

        j = { "mzJ`z", "Join" },
        k = { "i<cr><esc>", "Split" },
        J = { "<cmd>SplitjoinJoin<cr>", "Smart Join" },
        K = { "<cmd>SplitjoinSplit<cr>", "Smart Split" },

        r = { "<plug>(SubversiveSubstitute)", "Substitute" },
        t = { "<Plug>(EasyAlign)", "Easy Allign" },
        c = { "Comment" },
        O = { "O<Esc>", "Insert Blankline Before" },
        o = { "o<Esc>", "Insert Blankline" },

--         h = {
--             name = "Hug",

--             h = { "<cmd>set operatorfunc=SurroundAddOperatorMode<cr>g@", "Hug Around" },
--             H = { "<cmd>lua require'surround'.repeat_last()<cr>", "Hug Repeat" },
--             r = { "<cmd>lua require'surround'.surround_replace()", "Hug Replace" },
--             d = { "<cmd>lua require'surround'.surround_delete()<cr>", "Hug Delete" },
--         },

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
            w = { [["mz!ippar w". &textwidth . "<cr>`z"]], "Wrap Paragraph to Textwidth", expr = true },
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
        [">"] = { "<cmd>Telescope spell_suggest theme=get_cursor<cr>", "Spelling Suggestions" },
        F = { "<cmd>Telescope commands<cr>", "Commands" },
        f = {
            name = "Find",
            ["/"] = { "<cmd>Telescope search_history<cr>", "Search History" },
            [":"] = { "<cmd>Telescope command_history<cr>", "Search History" },
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
            Q = { "<cmd>Telescope loclist<cr>", "LocList" },
            q = { "<cmd>Telescope quickfix<cr>", "QuickFix" },
            r = { "<cmd>Telescope live_grep<cr>", "Grep" },
            R = {
                [["<cmd> Telescope grep_string search=" . input("Grep For > ") . "<CR>"]],
                "Fast Grep (with regex)",
                expr = true,
            },
            t = { "<cmd>Telescope treesitter<cr>", "Treesitter" },
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
            w = {
                [["<cmd> Cfind! " . input("What would you like to find? ")  . "<cr><cmd>Telescope quickfix<cr>"]],
                "Find",
                expr = true,
            },
            W = {
                [["<cmd> Clocate! " . input("What would you like to locate? ")  . "<cr><cmd>Telescope quickfix<cr>"]],
                "Locate",
                expr = true,
            },
            y = { "<cmd>Telescope registers<cr>", "All Registers" },
            x = { "<cmd>Telescope file_browser<cr>", "File Browser" },
            I = { "<cmd>lua require'telescope.builtin'.symbols{sources={'julia'}}<cr>", "Insert Symbols" },
            -- z = { "<cmd>Telescope session-lens search_session<cr>", "Session Search" },
        },
        g = {
            name = "Git",
            g = { "<cmd>silent !kitty @ launch --type=window --window-title 'Lazygit' lazygit<cr>", "Neogit Status" },

            a = { "<cmd>lua require'gitsigns'.blame_line({full=true})<CR>", "Blame Line" },
            A = { "<cmd>Gitsigns toggle_current_line_blame<CR>", "Blame Toggle" },

            b = { "<cmd>Telescope git_branches<cr>", "Branches" },
            C = { "<cmd>Telescope git_bcommits<cr>", "Commits (buffer)" },
            c = { "<cmd>Telescope git_commits<cr>", "Commits" },

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
                "<cmd>lua vim.diagnostic.show_line_diagnostics({ focusable = false, border='single'})<CR>",
                "Diagnostics",
            },
            W = { "<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", "Workspace Directory" },
            l = { "<cmd>LspInfo<cr>", "Lsp Infomation" },
            L = { "<cmd>NullLsInfo<cr>", "Null Ls  Info" },
            g = { "<cmd>lua require'gitsigns'.preview_hunk()<CR>", "Hunk Preview" },
            w = { "<cmd>MatchupWhereAmI??<cr>", "Preview Location" },
        },
        v = {
            name = "View",
            v = { "v:lua.commandRepeat('<leader>v', 'panelRepeat')", "Repeat Panel", expr = true, noremap = false },
            e = { "<cmd>let g:panelRepeat='e'<cr><cmd>TroubleToggle lsp_workspace_diagnostics<CR>", "Error List" },
            E = { "<cmd>let g:panelRepeat='E'<cr><cmd>TroubleToggle lsp_document_diagnostics<CR>", "Error List (buffer)" },
            q = { "<cmd>let g:panelRepeat='q'<cr><cmd>TroubleToggle quickfix<CR>", "QuickFix List" },
            l = { "<cmd>let g:panelRepeat='l'<cr><cmd>TroubleToggle loclist<CR>", "Location List" },
            f = { "<cmd>let g:panelRepeat='f'<cr><cmd>TroubleToggle telescope<CR>", "Telescope List" },
            n = { "<cmd>let g:panelRepeat='n'<cr><cmd>TodoTrouble<cr>", "Todo List" },
            g = { "<cmd>let g:panelRepeat='g'<cr><cmd>DiffviewOpen<CR>", "Git" },
            o = { "<cmd>let g:panelRepeat='o'<cr><cmd>SymbolsOutline<CR>", "Symbol List" },
            x = { "<cmd>let g:panelRepeat='x'<cr><cmd>NvimTreeToggle<CR>", "File Tree" },
            u = { "<cmd>let g:panelRepeat='u'<cr><cmd>UndotreeToggle<CR>", "Undo Tree" },
            z = {
                d = { "<cmd>let g:panelRepeat='zd'<cr><cmd>TroubleToggle lsp_definitions<cr>", "List Definitions" },
                i = {
                    "<cmd>let g:panelRepeat='zi'<cr><cmd>TroubleToggle lsp_implementations<cr>",
                    "List Implementations",
                },
                r = { "<cmd>let g:panelRepeat='zr'<cr><cmd>TroubleToggle lsp_references<cr>", "List References" },
            },
            j = { "<cmd>MagmaShowOutput<cr>", "Evaluate Line" },
        },
        Q = { "<cmd>CClear<cr><cmd>cgetbuffer<cr><cmd>TroubleRefresh<cr>", "Populater QF List With Buffer Errors " },
        q = {
            name = "QuickFix List",
            a = { "<cmd>caddbuffer<cr><cmd>TroubleRefresh<cr>", "Add Buffer Errrors to QF List" },
            c = { "<cmd>CClear<cr><cmd>TroubleRefresh<cr>", "Clear The List" },
            g = {
                "<cmd>Gitsigns setqflist<cr><cmd>let g:panelRepeat='q'<cr><cmd>Trouble quickfix<cr>",
                "Populate With Diffs",
            },
            ["<down>"] = { "<cmd>cnewer<cr>", "Newer List" },
            ["<up>"] = { "<cmd>colder<cr>", "Older List" },
            q = { "<cmd>let g:panelRepeat='q'<cr><cmd>TroubleToggle quickfix<CR>", "QuickFix List", noremap = false },
            n = { "<cmd>TodoQuickFix<cr>", "Populate With Todo Comments" },
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
            w = {
                [["<cmd> Cfind! " . input("What would you like to find? ")  . "<cr><cmd>let g:panelRepeat='q'<cr><cmd>Trouble quickfix<cr>"]],
                "Populate With find",
                expr = true,
            },
            W = {
                [["<cmd> Clocate! " . input("What would you like to locate? ")  . "<cr><cmd>let g:panelRepeat='q'<cr><cmd>Trouble quickfix<cr>"]],
                "Populate With Locate",
                expr = true,
            },
        },
        L = { "<cmd>LClear<cr><cmd>lgetbuffer<cr><cmd>TroubleRefresh<cr>", "Populater LocList With Buffer Errors " },
        l = {
            name = "Location List",
            a = { "<cmd>laddbuffer<cr><cmd>TroubleRefresh<cr>", "Add Buffer Errrors to LocList" },
            c = { "<cmd>LClear<cr><cmd>TroubleRefresh<cr>", "Clear The List" },
            l = { "<cmd>let g:panelRepeat='l'<cr><cmd>TroubleToggle loclist<CR>", "Location List" },
            g = {
                "<cmd>Gitsigns setloclist<cr><cmd>let g:panelRepeat='q'<cr><cmd>Trouble quickfix<cr>",
                "Populate With Diffs",
            },
            ["<up>"] = { "<cmd>lnewer<cr>", "Newer List" },
            ["<down>"] = { "<cmd>lolder<cr>", "Older List" },
            n = { "<cmd>TodoLocList<cr>", "Populate With Todo Comments" },
            V = {
                [["<cmd> noautocmd lvimgrep /" . input("What would you like to vimgrep? > ") . "/gj **/* <cr><cmd>let g:panelRepeat='l'<cr><cmd>Trouble loclist<cr>"]],
                "Populate With VimGrep (file select)",
                expr = true,
            },
            v = {
                [["<cmd> noautocmd lvimgrep /" . input("What would you like to vimgrep? > ") . "/gj " . input("In what files? > ") . "<cr><cmd>let g:panelRepeat='l'<cr><cmd>Trouble loclist<cr>"]],
                "Populate With VimGrep",
                expr = true,
            },
            w = {
                [["<cmd> Lfind! " . input("What would you like to find? ")  . "<cr><cmd>let g:panelRepeat='l'<cr><cmd>Trouble loclist<cr>"]],
                "Populate With find",
                expr = true,
            },
            W = {
                [["<cmd> Llocate! " . input("What would you like to locate? ")  . "<cr><cmd>let g:panelRepeat='l'<cr><cmd>Trouble loclist<cr>"]],
                "Populate With Locate",
                expr = true,
            },
        },
        E = { "<cmd>CClear<cr><cmd>cgetbuffer<cr><cmd>TroubleRefresh<cr>", "Open Buffre Errors in Touble" },
        e = {
            name = "Errors",
            p = { "<cmd>call v:lua.toggle_diagnostics()<cr>", "Toggle Diagnostics Shown" },
            r = { "<cmd>TroubleRefresh<cr>", "Refresh Errors" },
            e = { "<cmd>let g:panelRepeat='e'<cr><cmd>TroubleToggle lsp_workspace_diagnostics<CR>", "Error List" },
            E = { "<cmd>let g:panelRepeat='E'<cr><cmd>TroubleToggle lsp_document_diagnostics<CR>", "Error List (buffer)" },
            f = { "<cmd>let g:panelRepeat='f'<cr><cmd>TroubleToggle telescope<CR>", "Telescope List" },
            n = { "<cmd>let g:panelRepeat='n'<cr><cmd>TodoTrouble<cr>", "Todo List" },
        },
        t = {
            name = "Terminal",
            t = { "<cmd>silent !kittyterm normterm<cr>", "Normal Terminal" },
            i = { [[<cmd>silent exe "!kittyrepl replterm " . b:replCommand<cr>]], "REPL Terminal" },
            d = { [[<cmd>silent exe "!kittycommand debugterm " . b:debugCommand<cr>]], "Debug Terminal" },
            m = { "<cmd>silent !kittyterm maketerm<cr>", "Building Terminal" },
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
        J = { "<cmd>MagmaInit<cr>", "Start Jupyter" },
        j = {
            name = "Jupyter",
            j = { "<cmd>MagmaEvaluateLine<cr>", "Evaluate Line" },
            r = { "<cmd>MagmaReevaluateCell<cr>", "Re-Evaluate Cell" },
            d = { "<cmd>MagmaDelete<cr>", "Delete Cell" },
        },
        r = {
            name = "Refactor",
            t = {"<cmd>lua require('neogen').generate({type = 'type' })<cr>", "Type Documentation"},
            c = {"<cmd>lua require('neogen').generate({type = 'class' })<cr>", "Class Documentation"},
            f = {"<cmd>lua require('neogen').generate({type = 'func' })<cr>", "Function Documentation"},
            v = { "<plug>(ExtractVar)", "Extract Variable" },
            r = { "<cmd>lua vim.lsp.buf.rename()<CR>", "Rename (LSP)" },
            R = { "Rename (Treesitter)" },
        },
        x = {
            name = "Explorer",
            x = { "<cmd>let g:panelRepeat='x'<cr><cmd>NvimTreeToggle<CR>", "File Tree" },
        },
        w = {
            name = "Window Managment",
            ["<leader>"] = { "<c-w>p", "Jump To Last Split" },
            O = { "<cmd>BDelete hidden<cr>", "Close All Hidden Buffers" },
            f = { "<c-w>w", "Focus Floating Window" },
            d = { "<cmd>bdelete<cr>", "Delete the current buffer" },
            D = { "<cmd>tabclose<cr>", "Close the Current Tab" },
            w = { "<cmd>ZenMode<cr>", "Zen Mode" },
            o = { "<c-w>o", "Clean Up Windows" },
            ["<bs>"] = { "<c-w>c", "Close Window" },
            ["<cr>"] = { "<c-w>v", "Open Window" },
            x = { "<c-w>s", "Horizontal Split" },
            v = { "<c-w>v", "Vertical Split" },
            n = { "<C-W>n", "Open New Window" },
            N = { "<C-W>r", "Move Window Next" },
            P = { "<C-W>R", "Move Window Previous" },
            ["]"] = { "<cmd>vertical resize +5<cr>", "Vertical Resize" },
            ["["] = { "<cmd>vertical resize -5<cr>", "Vertical Resize" },
            ["}"] = { "<cmd>resize +5<cr>", "Horizontal Resize" },
            ["{"] = { "<cmd>resize -5<cr>", "Horizontal Resize" },
            ["="] = { "<c-w>=", "Equal Size" },
            h = { "<c-w>h", "Left Windown" },
            j = { "<c-w>j", "Below Window" },
            k = { "<c-w>k", "Above Window" },
            l = { "<c-w>l", "Right Window" },
            ["<left>"] = { "<cmd>lua require('swap-buffers').swap_buffers('h')<cr>", "Swap Left" },
            ["<down>"] = { "<cmd>lua require('swap-buffers').swap_buffers('j')<cr>", "Swap Down" },
            ["<up>"] = { "<cmd>lua require('swap-buffers').swap_buffers('k')<cr>", "Swap Up" },
            ["<right>"] = { "<cmd>lua require('swap-buffers').swap_buffers('l')<cr>", "Swap Right" },
            H = { "<c-w>H", "Move Far Left" },
            J = { "<c-w>J", "Move Far Down" },
            K = { "<c-w>K", "Move Far Up" },
            L = { "<c-w>L", "Move Far Right" },
            c = { "<c-w>c", "Close Window" },
            q = { "<c-w>c", "Close Window" },
            ["/"] = { "<c-w>^", "Open Alternate File" },
            [","] = { "<cmd>BufferLineCyclePrev<cr>", "Previous Buffer" },
            ["."] = { "<cmd>BufferLineCycleNext<cr>", "Next Buffer" },
        },
        [","] = {
            name = "Settings",
            [","] = { "<cmd>Telescope vim_options<cr>", "Vim Options" },
            s = { "<cmd>set spell!<cr>", "Toggle Spelling" },
            k = { "<cmd>Telescope keymaps<cr>", "Keymaps" },
            c = { "<cmd>Telescope colorscheme<cr>", "Color Schemes" },
            C = { "<cmd>Telescope highlights<cr>", "Highlight Groups" },
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
        h = {
            h = { "<cmd>Telescope help_tags<cr>", "Search Help Tags" },
            i = { "<cmd>LvimHelper<cr>", "Insert Mode Mappings" },
            k = { "K", "Documentation" },
        },
        k = {
            [[":silent !kitty @ launch --type=tab --tab-title 'Kak %:t' kak %:p +" . line(".") . ":" . col(".") . "<cr>"]],
            "Open file in Kak",
            expr = true,
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
        m = { "<cmd>let g:dirJumps='m'<cr>m`[`zz", "File Marks" },
        e = {
            "<cmd>lua vim.diagnostic.goto_prev({ focusable = false , popup_opts = { border = 'single' }})<CR>zz<cmd>let g:dirJumps='e'<cr>m`",
            "Diagnostics",
        },
        E = {
            "<cmd>lua vim.diagnostic.goto_prev({ severity='Error', focusable = false , popup_opts = { border = 'single' }})<CR>zz<cmd>let g:dirJumps='E'<cr>m`",
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
        m = { "<cmd>let g:dirJumps='m'<cr>m`]`zz", "File Marks" },
        e = {
            "<cmd>lua vim.diagnostic.goto_next({ focusable = false , popup_opts = { border = 'single' }})<CR>zz<cmd>let g:dirJumps='e'<cr>m`",
            "Diagnostics",
        },
        E = {
            "<cmd>lua vim.diagnostic.goto_next({ severity='Error', focusable = false , popup_opts = { border = 'single' }})<CR>zz<cmd>let g:dirJumps='E'<cr>m`",
            "Error",
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
