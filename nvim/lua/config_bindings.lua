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
            enabled = true,
            suggestions = 20,
        },
        presets = {
            operators = true,
            motions = true,
            text_objects = true,
            windows = true,
            nav = true,
            z = true,
            g = true,
        },
    },
    operators = { gc = "Comments" },
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
        spacing = 3,
    },
    hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ " },
    show_help = true,
})

-- Normal Bindings
require("which-key").register({
    g = {
        J = { "<cmd>SplitjoinJoin<cr>", "Smart Join" },
        K = { "<cmd>SplitjoinSplit<cr>", "Smart Split" },
        R = { "<plug>(SubversiveSubstituteToEndOfLine)", "Substitute to EOL" },
        r = { "<plug>(SubversiveSubstitute)", "Substitute" },
        rR = { "<plug>(SubversiveSubstitute)^", "Substitute to SOL" },
        rr = { "<plug>(SubversiveSubstituteLine)", "Substitute Line" },
        [":"] = { "Q", "Ex Mode" },
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
        ["."] = {
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
        O = { "O<Esc>", "Insert Blankline Before" },
        o = { "o<Esc>", "Insert Blankline" },
        j = { "mzJ`z", "Join" },
        k = { "i<cr><esc>", "Split" },
        t = { "<Plug>(EasyAlign)", "Easy Allign" },
        i = { "<Plug>(ninja-insertstart)", "Insert in Object" },
        a = { "<Plug>(ninja-insertend)", "Append to Object" },
        c = { "Comment" },
        cc = { "Comment Line" },
        P = {
            name = "Paste Before",
            b = { "<Plug>UnconditionalPasteBlockBefore", "Paste Block" },
            B = { "<Plug>UnconditionalPasteJaggedBefore", "Paste Jagged" },
            I = { "<Plug>UnconditionalPasteCharBefore", "Paste Char" },
            i = { "<Plug>UnconditionalPasteInlinedBefore", "Paste Inline" },
            l = { "<plug>UnconditionalPasteLineBefore", "Paste Line" },
            S = { "<Plug>UnconditionalPasteParagraphedBefore", "Paste Paragraph" },
            s = { "<Plug>UnconditionalPasteSpacedBefore", "Paste Spaced" },
        },
        p = {
            name = "Paste After",
            b = { "<Plug>UnconditionalPasteBlockAfter", "Paste Block" },
            B = { "<Plug>UnconditionalPasteJaggedAfter", "Paste Jagged" },
            I = { "<Plug>UnconditionalPasteCharAfter", "Paste Char" },
            i = { "<Plug>UnconditionalPasteInlinedAfter", "Paste Inline" },
            l = { "<plug>UnconditionalPasteLineAfter", "Paste Line" },
            S = { "<Plug>UnconditionalPasteParagraphedAfter", "Paste Paragraph" },
            s = { "<Plug>UnconditionalPasteSpacedAfter", "Paste Spaced" },
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
            s = {
                name = "Change Case (Line)",
                p = { "gspix", "Pascal Case", noremap = false },
                c = { "gscix", "Camel Case", noremap = false },
                ["_"] = { "gs_ix", "Snake Case", noremap = false },
                u = { "gsuix", "Upper Case", noremap = false },
                t = { "gstix", "Title Case", noremap = false },
                d = { "gssix", "Sentance Case", noremap = false },
                ["<space>"] = { "gs<space>ix", "Space Case", noremap = false },
                ["-"] = { "gs-ix", "Kebab Case", noremap = false },
                k = { "gskix", "Title Kebab Case", noremap = false },
                ["."] = { "gs.ix", "Dot Case", noremap = false },
            },
            S = {
                name = "Change Case (SOL)",
                p = { "gspH", "Pascal Case", noremap = false },
                c = { "gscH", "Camel Case", noremap = false },
                ["_"] = { "gs_H", "Snake Case", noremap = false },
                u = { "gsuH", "Upper Case", noremap = false },
                t = { "gstH", "Title Case", noremap = false },
                d = { "gssH", "Sentance Case", noremap = false },
                ["<space>"] = { "gs<space>H", "Space Case", noremap = false },
                ["-"] = { "gs-H", "Kebab Case", noremap = false },
                k = { "gskH", "Title Kebab Case", noremap = false },
                ["."] = { "gs.H", "Dot Case", noremap = false },
            },
        },
        S = {
            name = "Change Case (EOL)",
            p = { "gspL", "Pascal Case", noremap = false },
            c = { "gscL", "Camel Case", noremap = false },
            ["_"] = { "gs_L", "Snake Case", noremap = false },
            u = { "gsuL", "Upper Case", noremap = false },
            t = { "gstL", "Title Case", noremap = false },
            d = { "gssL", "Sentance Case", noremap = false },
            ["<space>"] = { "gs<space>L", "Space Case", noremap = false },
            ["-"] = { "gs-L", "Kebab Case", noremap = false },
            k = { "gskL", "Title Kebab Case", noremap = false },
            ["."] = { "gs.L", "Dot Case", noremap = false },
        },
    },
    ["<leader>"] = {
        ["<leader>"] = { "<cmd>e #<cr>", "Last File" },
        ["/"] = {
            name = "Related Files",
            ["<leader>"] = { "<c-^>", "Last File" },
            ["/"] = { "<cmd>A<cr>", "Alternate File" },
            f = {
                [["<cmd>lua require'telescope.builtin'.find_files({find_command={'fd', \"" . split(expand("%:t:r"), '_')[0] . "\"}})<cr>"]],
                "Search",
                expr = true,
            },
            d = { "<cmd>Edoc<cr>", "Documentation" },
            D = { "<cmd>EmainDoc<cr>", "Main Documentation" },
            s = { "<cmd>Esource<cr>", "Source" },
            b = { "<cmd>Ebench<cr>", "Benchmark" },
            B = { "<cmd>EmainBench<cr>", "Main Benchmark" },
            t = { "<cmd>Etest<cr>", "Test" },
            T = { "<cmd>EmainTest<cr>", "Main Test" },
            p = { "<cmd>Edeps<cr>", "Project Dependencies" },
            r = { "<cmd>Ereadme<cr>", "Readme" },
            v = {
                name = "Vertical Split",
                ["<leader>"] = { "<cmd>vsplit #<cr>", "Last File" },
                ["/"] = { "<cmd>AV<cr>", "Alternate File" },
                d = { "<cmd>Vdoc<cr>", "Documentation" },
                D = { "<cmd>VmainDoc<cr>", "Main Documentation" },
                s = { "<cmd>Vsource<cr>", "Source" },
                b = { "<cmd>Vbench<cr>", "Benchmark" },
                B = { "<cmd>VmainBench<cr>", "Main Benchmark" },
                t = { "<cmd>Vtest<cr>", "Test" },
                T = { "<cmd>VmainTest<cr>", "Main Test" },
                p = { "<cmd>Vdeps<cr>", "Project Dependencies" },
                r = { "<cmd>Vreadme<cr>", "Readme" },
            },
            x = {
                name = "Horizontal Split",
                ["<leader>"] = { "<cmd>split #<cr>", "Last File" },
                ["/"] = { "<cmd>AS<cr>", "Alternate File" },
                d = { "<cmd>Sdoc<cr>", "Documentation" },
                D = { "<cmd>SmainDoc<cr>", "Main Documentation" },
                s = { "<cmd>Ssource<cr>", "Source" },
                b = { "<cmd>Sbench<cr>", "Benchmark" },
                B = { "<cmd>SmainBench<cr>", "Main Benchmark" },
                t = { "<cmd>Stest<cr>", "Test" },
                T = { "<cmd>SmainTest<cr>", "Main Test" },
                p = { "<cmd>Sdeps<cr>", "Project Dependencies" },
                r = { "<cmd>Sreadme<cr>", "Readme" },
            },
            O = {
                name = "New Tab",
                ["<leader>"] = { "<cmd>tabedit #<cr>", "Last File" },
                ["/"] = { "<cmd>AT<cr>", "Alternate File" },
                d = { "<cmd>Tdoc<cr>", "Documentation" },
                D = { "<cmd>TmainDoc<cr>", "Main Documentation" },
                s = { "<cmd>Tsource<cr>", "Source" },
                b = { "<cmd>Tbench<cr>", "Benchmark" },
                B = { "<cmd>TmainBench<cr>", "Main Benchmark" },
                t = { "<cmd>Ttest<cr>", "Test" },
                T = { "<cmd>TmainTest<cr>", "Main Test" },
                p = { "<cmd>Tdeps<cr>", "Project Dependencies" },
                r = { "<cmd>Treadme<cr>", "Readme" },
            },
            n = {
                name = "New File",
                d = { [["<cmd>Edoc " . input('File Name > ') . "<cr>"]], "Documentation", expr = true },
                D = { [[<cmd>EmainDoc<cr>]], "Main Documentation" },
                s = { [["<cmd>Esource " . input('File Name > ') . "<cr>"]], "Source", expr = true },
                b = { [["<cmd>Ebench " . input('File Name > ') . "<cr>"]], "Benchmark", expr = true },
                B = { [[<cmd>EmainBench<cr>]], "Main Benchmarks" },
                t = { [["<cmd>Etest " . input('File Name > ') . "<cr>"]], "Test", expr = true },
                T = { [[<cmd>EmainTest<cr>]], "Main Tests" },
                p = { [[<cmd>Edeps<cr>]], "Project Dependencies" },
                r = { [[<cmd>Ereadme<cr>]], "Readme" },
            },
        },
        [">"] = { "<cmd>Telescope spell_suggest theme=get_cursor<cr>", "Spelling Suggestions" },
        o = {
            name = "Open",
            f = { "gf", "Open File" },
            t = { "gd", "Open Tag Deffinition" },
        },
        F = { "<cmd>Telescope commands<cr>", "Commands" },
        f = {
            name = "Find",
            ["/"] = { "<cmd>Telescope search_history<cr>", "Search History" },
            [":"] = { "<cmd>Telescope command_history<cr>", "Search History" },
            ["*"] = {
                [[<cmd>lua require'telescope.builtin'.find_files({find_command={'rg', vim.fn.expand("<cword>")}})<cr>]],
                "Grep Word Under Cursor",
            },
            B = { "<cmd>Telescope buffers only_cwd=true<cr>", "Buffers (cwd)" },
            b = { "<cmd>Telescope buffers<cr>", "Buffers" },
            C = { "<cmd>Telescope git_bcommits<cr>", "Commits (buffer)" },
            c = { "<cmd>Telescope git_commits<cr>", "Git Commits" },
            F = { "<cmd>lua require('telescope.builtin').find_files({find_command={'fd', '-I'}})<cr>", "Files (non git)" },
            f = { "<cmd>lua require'config_telescope'.project_files()<cr>", "Find Files" },
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
            y = { "<cmd>Telescope registers<cr>", "Registers" },
            x = { "<cmd>Telescope file_browser<cr>", "File Browser" },
            I = { "<cmd>lua require'telescope.builtin'.symbols{sources={'julia'}}<cr>", "Insert Symbols" },
            -- z = { "<cmd>Telescope session-lens search_session<cr>", "Session Search" },
        },
        G = { "v:lua.commandRepeat('<leader>g', 'gitRepeat')", noremap = false, expr = true },
        g = {
            name = "Git",
            a = { "<cmd>Gitsigns blame_line<CR>", "Blame Line" },
            A = { "<cmd>Gitsigns toggle_current_line_blame<CR>", "Blame Toggle" },
            b = { "<cmd>Telescope git_branches<cr>", "Branches" },
            C = { "<cmd>Telescope git_bcommits<cr>", "Commits (buffer)" },
            c = { "<cmd>Telescope git_commits<cr>", "Commits" },
            d = {
                d = { "<cmd>DiffviewOpen<CR>", "Git Diff Viewer" },
                D = { "<cmd>Gitsigns diffthis", "Diff View Based On Signs" },
                c = { "<cmd>call v:lua.git_commits_againsthead()<cr>", "View The Diff of a Commit" },
                C = { "<cmd>call v:lua.git_commits_onechange()<cr>", "Diff Against a Commit" },
                b = { "<cmd>call v:lua.git_branch_dif()<cr>", "View The Diff of a Branch" },
                B = { "<cmd>call v:lua.git_branch_mergebase()<cr>", "Diff Against a Branch" },
            },
            g = { "<cmd>Neogit<cr>", "Neogit Status" },
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
            g = { "<cmd>lua require'gitsigns'.preview_hunk()<CR>", "Hunk Preview" },
            w = { "<cmd>MatchupWhereAmI??<cr>", "Preview Location" },
            E = { "<cmd>call v:lua.toggle_diagnostics()<cr>", "Toggle Diagnostics Shown" },
        },
        v = {
            name = "View",
            v = { "v:lua.commandRepeat('<leader>v', 'panelRepeat')", "Repeat Panel", expr = true, noremap = false },
            e = { "<cmd>let g:panelRepeat='e'<cr><cmd>Trouble lsp_workspace_diagnostics<CR>", "Error List" },
            E = { "<cmd>let g:panelRepeat='E'<cr><cmd>Trouble lsp_document_diagnostics<CR>", "Error List (buffer)" },
            q = { "<cmd>let g:panelRepeat='q'<cr><cmd>Trouble quickfix<CR>", "QuickFix List" },
            l = { "<cmd>let g:panelRepeat='l'<cr><cmd>Trouble loclist<CR>", "Location List" },
            f = { "<cmd>let g:panelRepeat='f'<cr><cmd>Trouble telescope<CR>", "Telescope List" },
            n = { "<cmd>let g:panelRepeat='n'<cr><cmd>TodoTrouble<cr>", "Todo List" },
            g = { "<cmd>let g:panelRepeat='g'<cr><cmd>DiffviewOpen<CR>", "Git" },
            i = { "<cmd>let g:panelRepeat='i'<cr><cmd>3ToggleTerm<cr>", "REPL Terminal" },
            m = { "<cmd>let g:panelRepeat='m'<cr><cmd>2ToggleTerm<cr>", "Build Terminal" },
            L = { "<cmd>let g:panelRepeat='L'<cr><cmd>lopen<CR>", "Better Location List" },
            Q = { "<cmd>let g:panelRepeat='Q'<cr><cmd>copen<CR>", "Better QuickFix List" },
            o = { "<cmd>let g:panelRepeat='o'<cr><cmd>SymbolsOutlineOpen<CR>", "Symbol List" },
            x = { "<cmd>let g:panelRepeat='x'<cr><cmd>NvimTreeOpen<CR>", "File Tree" },
            T = { "<cmd>let g:panelRepeat='T'<cr><cmd>ToggleTermOpenAll<CR>", "All Terminals" },
            t = { "<cmd>let g:panelRepeat='t'<cr><cmd>1ToggleTerm<cr>", "Terminal" },
            u = { "<cmd>let g:panelRepeat='u'<cr><cmd>UndotreeShow<CR>", "Undo Tree" },
        },
        Q = { "<cmd>CClear<cr><cmd>cgetbuffer<cr><cmd>TroubleRefresh<cr>", "Populater QF List With Buffer Errors " },
        q = {
            name = "QuickFix List",
            a = { "<cmd>caddbuffer<cr><cmd>TroubleRefresh<cr>", "Add Buffer Errrors to QF List" },
            c = { "<cmd>CClear<cr><cmd>TroubleRefresh<cr>", "Clear The List" },
            g = { "<cmd>Gitsigns setqflist<cr><cmd>Trouble quickfix<cr>", "Populate With Diffs" },
            ["<down>"] = { "<cmd>cnewer<cr>", "Newer List" },
            ["<up>"] = { "<cmd>colder<cr>", "Older List" },
            q = { "<cmd>let g:panelRepeat='q'<cr><cmd>Trouble quickfix<CR>", "QuickFix List" },
            n = { "<cmd>TodoQuickFix<cr>", "Populate With Todo Comments" },
            f = { "<cmd>let g:panelRepeat='Q'<cr><cmd>copen<CR>", "Better QuickFix List" },
            V = {
                [["<cmd> noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj **/* <cr><cmd>Trouble quickfix<cr>"]],
                "Populate With VimGrep (file select)",
                expr = true,
            },
            v = {
                [["<cmd> noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj " . input("In what files? > ") . "<cr><cmd>Trouble quickfix<cr>"]],
                "Populate With VimGrep",
                expr = true,
            },
            w = {
                [["<cmd> Cfind! " . input("What would you like to find? ")  . "<cr><cmd>Trouble quickfix<cr>"]],
                "Populate With find",
                expr = true,
            },
            W = {
                [["<cmd> Clocate! " . input("What would you like to locate? ")  . "<cr><cmd>Trouble quickfix<cr>"]],
                "Populate With Locate",
                expr = true,
            },
        },
        L = { "<cmd>LClear<cr><cmd>lgetbuffer<cr><cmd>TroubleRefresh<cr>", "Populater LocList With Buffer Errors " },
        l = {
            name = "Location List",
            a = { "<cmd>laddbuffer<cr><cmd>TroubleRefresh<cr>", "Add Buffer Errrors to LocList" },
            c = { "<cmd>LClear<cr><cmd>TroubleRefresh<cr>", "Clear The List" },
            l = { "<cmd>let g:panelRepeat='l'<cr><cmd>Trouble loclist<CR>", "Location List" },
            f = { "<cmd>let g:panelRepeat='L'<cr><cmd>lopen<CR>", "Better Location List" },
            g = { "<cmd>Gitsigns setloclist<cr><cmd>Trouble quickfix<cr>", "Populate With Diffs" },
            ["<up>"] = { "<cmd>lnewer<cr>", "Newer List" },
            ["<down>"] = { "<cmd>lolder<cr>", "Older List" },
            n = { "<cmd>TodoLocList<cr>", "Populate With Todo Comments" },
            V = {
                [["<cmd> noautocmd lvimgrep /" . input("What would you like to vimgrep? > ") . "/gj **/* <cr><cmd>Trouble loclist<cr>"]],
                "Populate With VimGrep (file select)",
                expr = true,
            },
            v = {
                [["<cmd> noautocmd lvimgrep /" . input("What would you like to vimgrep? > ") . "/gj " . input("In what files? > ") . "<cr><cmd>Trouble loclist<cr>"]],
                "Populate With VimGrep",
                expr = true,
            },
            w = {
                [["<cmd> Lfind! " . input("What would you like to find? ")  . "<cr><cmd>Trouble loclist<cr>"]],
                "Populate With find",
                expr = true,
            },
            W = {
                [["<cmd> Llocate! " . input("What would you like to locate? ")  . "<cr><cmd>Trouble loclist<cr>"]],
                "Populate With Locate",
                expr = true,
            },
        },
        E = { "<cmd>CClear<cr><cmd>cgetbuffer<cr><cmd>TroubleRefresh<cr>", "Open Buffre Errors in Touble" },
        e = {
            name = "Errors",
            r = { "<cmd>TroubleRefresh<cr>", "Refresh Errors" },
            e = { "<cmd>let g:panelRepeat='e'<cr><cmd>Trouble lsp_workspace_diagnostics<CR>", "Error List" },
            E = { "<cmd>let g:panelRepeat='E'<cr><cmd>Trouble lsp_document_diagnostics<CR>", "Error List (buffer)" },
            f = { "<cmd>let g:panelRepeat='f'<cr><cmd>Trouble telescope<CR>", "Telescope List" },
            n = { "<cmd>let g:panelRepeat='n'<cr><cmd>TodoTrouble<cr>", "Todo List" },
        },
        t = {
            name = "Terminal",
            t = { "<cmd>1ToggleTerm<cr>", "Open Terminal" },
            q = { [[<cmd>1TermExec cmd="exit"<CR>]], "Exit Terminal" },
            Q = {
                [[<cmd>1TermExec cmd="exit"<CR><cmd>2TermExec cmd="exit"<CR><cmd>3TermExec cmd="exit"<cr>]],
                "Exit All Terminals",
            },
            i = {
                "<cmd>1ToggleTerm<cr><cmd>2ToggleTerm<cr><cmd>3ToggleTerm<cr><cmd>ToggleTermCloseAll<cr>",
                "Initialize Terminals",
            },
        },
        i = {
            name = "REPL Terminal",
            i = { "<cmd>3ToggleTerm<CR>", "Open REPL" },
            q = { [[<cmd>3TermExec cmd="exit"<CR>]], "Exit REPL" },
        },
        m = {
            name = "Make Terminal",
            q = { [[<cmd>2TermExec cmd="exit"<cr>]], "Exit Make Terminal" },
        },
        u = {
            name = "tests",
            u = { "<cmd>4ToggleTerm<cr>", "Open Test Terminal" },
        },
        d = {
            name = "Debugging",
        },
        r = {
            name = "Refactor",
            t = { "Rename (Treesitter)" },
            v = { "<plug>(ExtractVar)", "Extract Variable" },
        },
        x = {
            name = "Explorer",
            x = { "<cmd>let g:panelRepeat='x'<cr><cmd>NvimTreeOpen<CR>", "File Tree" },
        },
        w = {
            name = "Window Managment",
            ["<leader>"] = { "<c-w>p", "Jump To Last Split" },
            O = { "<cmd>BDelete hidden<cr>", "Close All Hidden Buffers" },
            f = { "<cmd>vsplit<cr>gf", "Split Open Under Cursor" },
            d = { "<cmd>BDelete! this<cr>", "Delete the current buffer" },
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
            g = {
                name = "Git Settings",
                b = { "<cmd>call v:lua.gitsign_change_base()<cr>", "Change Gitsigns Base" },
                B = { "<cmd>call v:lua.gitsign_bchange_base()<cr>", "Change Gitsigns Base" },
            },
        },
        h = {
            h = { "<cmd>Telescope help_tags<cr>", "Search Help Tags" },
            i = { "<cmd>LvimHelper<cr>", "Insert Mode Mappings" },
            k = { "K", "Documentation" },
        },
        z = {
            d = { [[<cmd>%s/\v[^^ ]\zs  / /g<cr>]], "Remove Double Spaces" },
            w = { "<cmd>%!par w80<cr>", "Wrap File to 80 Characters" },
        },
    },
    ["["] = {
        name = "Backward Leader",
        ["["] = { "v:lua.commandRepeat('[', 'dirJumps')", "Repeat Last", expr = true, noremap = false },
        h = {
            [["<cmd>let g:dirJumps='h'<cr>" . &diff ? "[c" : "<cmd>lua require'gitsigns'.prev_hunk()<cr>" . "zz"]],
            "Hunk",
            expr = true,
        },
        Q = {
            "<cmd>let g:dirJumps='Q'<cr><cmd>try <bar> cpfile <bar> catch /E553/ <bar> clast <bar> endtry<cr>zz",
            "QuickFix File",
        },
        q = {
            "<cmd>let g:dirJumps='q'<cr><cmd>try <bar> cprevious <bar> catch /E553/ <bar> clast <bar> endtry<cr>zz",
            "QuickFix Entry",
        },
        s = { "<cmd>let g:dirJumps='s'<cr>[szz", "Spelling Mistake" },
        ["]"] = { "<cmd>let g:dirJumps=']'<cr>[]zz", "Section End", noremap = false },
        ["{"] = { "<cmd>let g:dirJumps='{'<cr>[[zz", "Section Start" },
        ["}"] = { "<cmd>let g:dirJumps='{'<cr>[[zz", "Section Start" },
        ["*"] = { "<cmd>let g:dirJumps='*'<cr>[#zz", "Function Call", noremap = false },
        o = { "<cmd>let g:dirJumps='o'<cr><cmd>TSTextobjectGotoPreviousStart @class.outer<cr>zz", "Class" },
        f = { "<cmd>let g:dirJumps='f'<cr><cmd>TSTextobjectGotoPreviousStart @function.outer<cr>zz", "Function" },
        [","] = { "<cmd>let g:dirJumps=','<cr><cmd>TSTextobjectGotoPreviousStart @parameter.inner<cr>zz", "Parameter" },
        c = { "<cmd>let g:dirJumps='c'<cr><cmd>TSTextobjectGotoPreviousStart @conditional.inner<cr>zz", "Conditional" },
        C = { "<cmd>let g:dirJumps='C'<cr><cmd>TSTextobjectGotoPreviousStart @comment.outer<cr>zz", "Comment" },
        l = { "<cmd>let g:dirJumps='l'<cr><cmd>TSTextobjectGotoPreviousStart @loop.outer<cr>zz", "Loop" },
        b = { "<cmd>let g:dirJumps='b'<cr><cmd>TSTextobjectGotoPreviousStart @block.outer<cr>zz", "Block" },
        O = { "<cmd>let g:dirJumps='O'<cr><cmd>TSTextobjectGotoPreviousEnd @class.outer<cr>zz", "Class" },
        F = { "<cmd>let g:dirJumps='F'<cr><cmd>TSTextobjectGotoPreviousEnd @function.outer<cr>zz", "Function" },
        ["<"] = { "<cmd>let g:dirJumps='<'<cr><cmd>TSTextobjectGotoPreviousEnd @parameter.inner<cr>zz", "Parameter" },
        L = { "<cmd>let g:dirJumps='L'<cr><cmd>TSTextobjectGotoPreviousEnd @loop.outer<cr>zz", "Loop" },
        B = { "<cmd>let g:dirJumps='B'<cr><cmd>TSTextobjectGotoPreviousEnd @block.outer<cr>zz", "Block" },
    },
    ["]"] = {
        name = "Forward Leader",
        ["]"] = { "v:lua.commandRepeat(']', 'dirJumps')", "Repeat Last", expr = true, noremap = false },
        h = {
            [["<cmd>let g:dirJumps='h'<cr>" . &diff ? "]c" : "<cmd>lua require'gitsigns'.next_hunk()<cr>" . "zz"]],
            "Hunk",
            expr = true,
        },
        Q = {
            "<cmd>let g:dirJumps='Q'<cr><cmd>try <bar> cnfile <bar> catch /E553/ <bar> cfirst <bar> endtry<cr>zz",
            "QuickFix File",
        },
        q = {
            "<cmd>let g:dirJumps='q'<cr><cmd>try <bar> cnext <bar> catch /E553/ <bar> cfirst <bar> endtry<cr>zz",
            "QuickFix Entry",
        },
        s = { "<cmd>let g:dirJumps='s'<cr>]szz", "Spelling Mistake" },
        ["["] = { "<cmd>let g:dirJumps='['<cr>][zz", "Section End", noremap = false },
        ["}"] = { "<cmd>let g:dirJumps='}'<cr>]]zz", "Section Start", noremap = true },
        ["{"] = { "<cmd>let g:dirJumps='}'<cr>]]zz", "Section Start", noremap = true },
        ["*"] = { "<cmd>let g:dirJumps='*'<cr>]#zz", "Function Call", noremap = false },
        o = { "<cmd>let g:dirJumps='o'<cr><cmd>TSTextobjectGotoNextStart @class.outer<cr>zz", "Class" },
        f = { "<cmd>let g:dirJumps='f'<cr><cmd>TSTextobjectGotoNextStart @function.outer<cr>zz", "Function" },
        [","] = { "<cmd>let g:dirJumps=','<cr><cmd>TSTextobjectGotoNextStart @parameter.inner<cr>zz", "Parameter" },
        c = { "<cmd>let g:dirJumps='c'<cr><cmd>TSTextobjectGotoNextStart @conditional.inner<cr>zz", "Conditional" },
        C = { "<cmd>let g:dirJumps='C'<cr><cmd>TSTextobjectGotoNextStart @comment.outer<cr>zz", "Comment" },
        l = { "<cmd>let g:dirJumps='l'<cr><cmd>TSTextobjectGotoNextStart @loop.outer<cr>zz", "Loop" },
        b = { "<cmd>let g:dirJumps='b'<cr><cmd>TSTextobjectGotoNextStart @block.outer<cr>zz", "Block" },
        O = { "<cmd>let g:dirJumps='O'<cr><cmd>TSTextobjectGotoNextEnd @class.outer<cr>zz", "Class" },
        F = { "<cmd>let g:dirJumps='F'<cr><cmd>TSTextobjectGotoNextEnd @function.outer<cr>zz", "Function" },
        ["<"] = { "<cmd>let g:dirJumps='<'<cr><cmd>TSTextobjectGotoNextEnd @parameter.inner<cr>zz", "Parameter" },
        L = { "<cmd>let g:dirJumps='L'<cr><cmd>TSTextobjectGotoNextEnd @loop.outer<cr>zz", "Loop" },
        B = { "<cmd>let g:dirJumps='B'<cr><cmd>TSTextobjectGotoNextEnd @block.outer<cr>zz", "Block" },
    },
    ["<localleader>"] = {
        name = "Local Leader",
    },
})

-- Visual Bindings

require("which-key").register({
    ["<leader>"] = {
        r = {
            name = "Refactor",
            v = { "<plug>(ExtractVarVis)", "Extract Variable" },
        },
        z = {
            w = { "!par w80<cr>", "Wrap to 80 Characters" },
            d = { [[:%s/\v[^^ ]\zs  / /g<cr>]], "Remove Double Spaces" },
        },
    },
    g = {
        R = { "<plug>(SubversiveSubstituteToEndOfLine)", "Substitute to EOL" },
        r = { "<plug>(SubversiveSubstitute)", "Substitute" },
        rr = { "<plug>(SubversiveSubstituteLine)", "Substitute Line" },
        rR = { "<plug>(SubversiveSubstitute)H", "Substitute to SOL" },
        j = { "J", "Join" },
        k = { "c<cr><esc>", "Split" },
        t = { "<Plug>(EasyAlign)", "Align" },
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
    z = {
        i = { "I", "Insert" },
        a = { "A", "Append" },
    },
}, {
    mode = "v",
})

-- set default movement
vim.api.nvim_set_var("dirJumps", "f")
vim.api.nvim_set_var("panelRepeat", "x")
vim.api.nvim_set_var("gitRepeat", "g")

function _G.commandRepeat(leader, varName)
    local jump = vim.api.nvim_get_var(varName)
    -- print(direction .. jump)
    return vim.api.nvim_replace_termcodes(leader .. jump, true, true, true)
end
