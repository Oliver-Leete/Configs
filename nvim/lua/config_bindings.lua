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
require("which-key").register({
    g = {
        J = {"<cmd>SplitjoinJoin<cr>", "Smart Join"},
        K = {"<cmd>SplitjoinSplit<cr>", "Smart Split"},
        R = {"<plug>(SubversiveSubstituteToEndOfLine)", "Substitute to EOL"},
        r = {"<plug>(SubversiveSubstitute)", "Substitute"},
        rR = {"<plug>(SubversiveSubstitute)^", "Substitute to SOL"},
        rr = {"<plug>(SubversiveSubstituteLine)", "Substitute Line"},
        [":"] = {"Q", "Ex Mode"},
        ["<"] = {"Swap With Previous Argument"},
        [">"] = {"Swap With Next Argument"},
        O = {"O<Esc>", "Insert Blankline Before"},
        o = {"o<Esc>", "Insert Blankline"},
        j = {"J", "Join"},
        k = {"i<cr><esc>", "Split"},
        t = {"<Plug>(EasyAlign)", "Easy Allign"},
        i = {"<Plug>(ninja-insertstart)", "Insert in Object"},
        a = {"<Plug>(ninja-insertend)", "Append to Object"},
        c = {"Comment"},
        cc = {"Comment Line"},
        P = {
            name = "Paste Before",
            b = {"<Plug>UnconditionalPasteBlockBefore", "Paste Block"},
            B = {"<Plug>UnconditionalPasteJaggedBefore", "Paste Jagged"},
            I = {"<Plug>UnconditionalPasteCharBefore", "Paste Char"},
            i = {"<Plug>UnconditionalPasteInlinedBefore", "Paste Inline"},
            l = {"<plug>UnconditionalPasteLineBefore", "Paste Line"},
            S = {"<Plug>UnconditionalPasteParagraphedBefore", "Paste Paragraph"},
            s = {"<Plug>UnconditionalPasteSpacedBefore", "Paste Spaced"},
        },
        p = {
            name = "Paste After",
            b = {"<Plug>UnconditionalPasteBlockAfter", "Paste Block"},
            B = {"<Plug>UnconditionalPasteJaggedAfter", "Paste Jagged"},
            I = {"<Plug>UnconditionalPasteCharAfter", "Paste Char"},
            i = {"<Plug>UnconditionalPasteInlinedAfter", "Paste Inline"},
            l = {"<plug>UnconditionalPasteLineAfter", "Paste Line"},
            S = {"<Plug>UnconditionalPasteParagraphedAfter", "Paste Paragraph"},
            s = {"<Plug>UnconditionalPasteSpacedAfter", "Paste Spaced"},
        },
        s = {
            name = "Change Case",
            p = {"<Plug>CaserMixedCase", "Pascal Case"},
            c = {"<Plug>CaserCamelCase", "Camel Case"},
            ["_"] = {"<Plug>CaserSnakeCase", "Snake Case"},
            u = {"<Plug>CaserUpperCase", "Upper Case"},
            t = {"<Plug>CaserTitleCase", "Title Case"},
            d = {"<Plug>CaserSentenceCase", "Sentance Case"},
            ["<space>"] = {"<Plug>CaserSpaceCase", "Space Case"},
            ["-"] = {"<Plug>CaserKebabCase", "Kebab Case"},
            k = {"<Plug>CaserTitleKebabCase", "Title Kebab Case"},
            ["."] = {"<Plug>CaserDotCase", "Dot Case"},
            s ={
                name = "Change Case (Line)",
                p = {"gspix", "Pascal Case", noremap=false},
                c = {"gscix", "Camel Case", noremap=false},
                ["_"] = {"gs_ix", "Snake Case", noremap=false},
                u = {"gsuix", "Upper Case", noremap=false},
                t = {"gstix", "Title Case", noremap=false},
                d = {"gssix", "Sentance Case", noremap=false},
                ["<space>"] = {"gs<space>ix", "Space Case", noremap=false},
                ["-"] = {"gs-ix", "Kebab Case", noremap=false},
                k = {"gskix", "Title Kebab Case", noremap=false},
                ["."] = {"gs.ix", "Dot Case", noremap=false},
            },
            S = {
                name = "Change Case (SOL)",
                p = {"gspH", "Pascal Case", noremap=false},
                c = {"gscH", "Camel Case", noremap=false},
                ["_"] = {"gs_H", "Snake Case", noremap=false},
                u = {"gsuH", "Upper Case", noremap=false},
                t = {"gstH", "Title Case", noremap=false},
                d = {"gssH", "Sentance Case", noremap=false},
                ["<space>"] = {"gs<space>H", "Space Case", noremap=false},
                ["-"] = {"gs-H", "Kebab Case", noremap=false},
                k = {"gskH", "Title Kebab Case", noremap=false},
                ["."] = {"gs.H", "Dot Case", noremap=false},
            },
        },
        S = {
            name = "Change Case (EOL)",
            p = {"gspL", "Pascal Case", noremap=false},
            c = {"gscL", "Camel Case", noremap=false},
            ["_"] = {"gs_L", "Snake Case", noremap=false},
            u = {"gsuL", "Upper Case", noremap=false},
            t = {"gstL", "Title Case", noremap=false},
            d = {"gssL", "Sentance Case", noremap=false},
            ["<space>"] = {"gs<space>L", "Space Case", noremap=false},
            ["-"] = {"gs-L", "Kebab Case", noremap=false},
            k = {"gskL", "Title Kebab Case", noremap=false},
            ["."] = {"gs.L", "Dot Case", noremap=false},
        },
    },
    ["<leader>"] = {
        ["<leader>"] = {"<c-^>", "Last File"},
        ["/"] = {
            name = "Related Files",
            ["<leader>"] = {"<c-^>", "Last File"},
            ["/"] = {"<cmd>A<cr>", "Alternate File"},
            f = {[["<cmd>lua require'telescope.builtin'.find_files({find_command={'fd', \"" . split(expand("%:t:r"), '_')[0] . "\"}})<cr>"]], "Search", expr=true},
            d = {"<cmd>Edoc<cr>", "Documentation"},
            s = {"<cmd>Esource<cr>", "Source"},
            b = {"<cmd>Ebench<cr>", "Benchmark"},
            t = {"<cmd>Etest<cr>", "Test"},
            p = {"<cmd>Edeps<cr>", "Project Dependencies"},
            r = {"<cmd>Ereadme<cr>", "Readme"},
            v = {
                name = "Vertical Split",
                ["<leader>"] = {"<cmd>vsplit #<cr>", "Last File"},
                ["/"] = {"<cmd>AV<cr>", "Alternate File"},
                d = {"<cmd>Vdoc<cr>", "Documentation"},
                s = {"<cmd>Vsource<cr>", "Source"},
                b = {"<cmd>Vbench<cr>", "Benchmark"},
                t = {"<cmd>Vtest<cr>", "Test"},
                p = {"<cmd>Vdeps<cr>", "Project Dependencies"},
                r = {"<cmd>Vreadme<cr>", "Readme"},
            },
            x = {
                name = "Horizontal Split",
                ["<leader>"] = {"<cmd>split #<cr>", "Last File"},
                ["/"] = {"<cmd>AS<cr>", "Alternate File"},
                d = {"<cmd>Sdoc<cr>", "Documentation"},
                s = {"<cmd>Ssource<cr>", "Source"},
                b = {"<cmd>Sbench<cr>", "Benchmark"},
                t = {"<cmd>Stest<cr>", "Test"},
                p = {"<cmd>Sdeps<cr>", "Project Dependencies"},
                r = {"<cmd>Sreadme<cr>", "Readme"},
            },
            T = {
                name = "New Tab",
                ["<leader>"] = {"<cmd>tabedit #<cr>", "Last File"},
                ["/"] = {"<cmd>AT<cr>", "Alternate File"},
                d = {"<cmd>Tdoc<cr>", "Documentation"},
                s = {"<cmd>Tsource<cr>", "Source"},
                b = {"<cmd>Tbench<cr>", "Benchmark"},
                t = {"<cmd>Ttest<cr>", "Test"},
                p = {"<cmd>Tdeps<cr>", "Project Dependencies"},
                r = {"<cmd>Treadme<cr>", "Readme"},
            },
            n = {
                name = "New File",
                d = {[["<cmd>Edoc " . input('File Name > ') . "<cr>"]], "Documentation", expr=true},
                s = {[["<cmd>Esource " . input('File Name > ') . "<cr>"]], "Source", expr=true},
                b = {[["<cmd>Ebench " . input('File Name > ') . "<cr>"]], "Benchmark", expr=true},
                t = {[["<cmd>Etest " . input('File Name > ') . "<cr>"]], "Test", expr=true},
                p = {[[<cmd>Edeps<cr>]], "Project Dependencies"},
                r = {[[<cmd>Ereadme<cr>]], "Readme"},
            },
        },
        [">"] = {"<cmd>Telescope spell_suggest theme=get_cursor<cr>", "Spelling Suggestions"},
        o = {
            name = "Open",
            f = {"gf", "Open File"},
        },
        F = {"<cmd>Telescope commands<cr>", "Commands"},
        f = {
            name = "Find",
            ["/"] = {"<cmd>Telescope search_history<cr>", "Search History"},
            [":"] = {"<cmd>Telescope command_history<cr>", "Search History"},
            ["*"] = {[[<cmd>lua require'telescope.builtin'.find_files({find_command={'rg', vim.fn.expand("<cword>")}})<cr>]], "Grep Word Under Cursor"},
            B = {"<cmd>Telescope buffers only_cwd=true show_all_buffers=true<cr>", "Buffers (cwd)"},
            b = {"<cmd>Telescope buffers show_all_buffers=true<cr>", "Buffers"},
            C = {"<cmd>Telescope git_bcommits<cr>", "Commits (buffer)"},
            c = {"<cmd>Telescope git_commits<cr>", "Git Commits"},
            E = {"<cmd>Telescope lsp_document_diagnostics<cr>", "Errors (buffer)"},
            e = {"<cmd>Telescope lsp_workspace_diagnostics<cr>", "Errors"},
            F = {"<cmd>lua require('telescope.builtin').find_files({find_command={'fd', '-I'}})<cr>", "Files (non git)"},
            -- G = {"<cmd>Git! difftool<cr><cmd>cclose<cr><cmd>Telescope quickfix<cr>", "Git Chunks"},
            g = {"<cmd>Telescope git_status<cr>", "Git Status"},
            j = {"<cmd>Telescope jumplist<cr>", "Jumps"},
            l = {"<cmd>Telescope current_buffer_fuzzy_find<cr>", "Fuzzy Line"},
            L = {"<cmd>Telescope grep_string only_sort_text=true search=\"\"<cr>", "Fuzzy Line (Project)"},
            m = {"<cmd>Telescope marks<cr>", "Marks"},
            n = {"<cmd>TodoTelescope<cr>", "Todo Items"},
            i = {"<cmd>Telescope media_files<cr>", "Images (and other media)"},
            o = {"<cmd>Telescope oldfiles<cr>", "Old Files"},
            Q = {"<cmd>Telescope loclist<cr>", "LocList"},
            q = {"<cmd>Telescope quickfix<cr>", "QuickFix"},
            r = {"<cmd>Telescope live_grep<cr>", "Grep"},
            R = {[["<cmd> Telescope grep_string search=" . input("Grep For > ") . "<CR>"]], "Fast Grep", expr=true},
            t = {"<cmd>Telescope treesitter<cr>", "Treesitter"},
            V = {[["<cmd> noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj " . input("In what files? > ") . "<cr><cmd>Telescope quickfix<cr>"]], "Vim Grep (file select)", expr=true},
            v = {[["<cmd> noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj **/* <cr><cmd>Telescope quickfix<cr>"]], "Vim Grep", expr=true},
            w = {[["<cmd> Cfind! " . input("What would you like to find? ")  . "<cr><cmd>Telescope quickfix<cr>"]], "Find", expr=true},
            W = {[["<cmd> Clocate! " . input("What would you like to locate? ")  . "<cr><cmd>Telescope quickfix<cr>"]], "Locate", expr=true},
            y = {"<cmd>Telescope registers<cr>", "Registers"},
            z = {"<cmd>Telescope session-lens search_session<cr>", "Session Search"},
        },
        G = {
            name = "GitHub",
            i = {"<cmd>Telescope gh issues<cr>", "Search Issues"},
            p = {"<cmd>Telescope gh pull_request<cr>", "Search Pull Requests"},
            g = {"<cmd>Telescope gh gist<cr>", "Search Gists"},
            r = {"<cmd>Telescope gh run<cr>", "Search GH Runs"},
        },
        g = {
            name = "Git",
            a = {"<cmd>Gitsigns blame_line<CR>", "Blame Line"},
            A = {"<cmd>Gitsigns toggle_current_line_blame<CR>", "Blame Toggle"},
            b = {"<cmd>Telescope git_branches<cr>", "Branches"},
            C = {"<cmd>Telescope git_bcommits<cr>", "Commits (buffer)"},
            c = {"<cmd>Telescope git_commits<cr>", "Commits"},
            d = {
                d = {"<cmd>call PMToggleView('gitdiff')<CR>", "Git Diff Viewer"},
                D = {"<cmd>Gitsigns diffthis", "Diff View of Signs"},
                c = {"<cmd>call v:lua.git_commits_again()<cr>", "View The Diff of a Commit"},
                C = {"<cmd>call v:lua.git_commits_compe()<cr>", "Diff Against a Commit"},
                b = {"<cmd>call v:lua.git_branch_again()<cr>", "View The Diff of a Branch"},
                B = {"<cmd>call v:lua.git_branch_compe()<cr>", "Diff Against a Branch"},
            },
            g = {"<cmd>Neogit<cr>", "Neogit Status"},
            p = {"<cmd>Gitsigns preview_hunk<CR>", "Hunk Preview"},
            r = {"<cmd>Gitsigns reset_hunk<CR>", "Hunk Reset"},
            R = {"<cmd>Gitsigns reset_buffer<CR>", "Blame Toggle"},
            S = {"<cmd>Gitsigns stage_buffer<CR>", "Stage File"},
            s = {"<cmd>Gitsigns stage_hunk<CR>", "Hunk Stage"},
            v = {"<cmd>Gitsigns select_hunk<CR>", "Select Current Hunk"},
            [","] = {
                name = "Settings",
                b = {"<cmd>call v:lua.gitsign_change_base()<cr>", "Change Gitsigns Base"},
                B = {"<cmd>call v:lua.gitsign_bchange_base()<cr>", "Change Gitsigns Base"},
            }
        },
        p = {
            name = "Preview",
            g = {"<cmd>lua require'gitsigns'.preview_hunk()<CR>", "Hunk Preview"},
            w = {"<cmd>MatchupWhereAmI??<cr>", "Preview Location"}
        },
        v = {
            name = "View",
            e = {"<cmd>call PMToggleView('errorlist')<CR>", "Error List"},
            E = {"<cmd>call PMToggleView('errorlistdoc')<CR>", "Error List (buffer)"},
            g = {"<cmd>call PMToggleView('gitdiff')<CR>", "Git"},
            i = {"<cmd>3ToggleTerm<cr>", "REPL Terminal"},
            m = {"<cmd>2ToggleTerm<cr>", "Build Terminal"},
            l = {"<cmd>call PMToggleView('loclist')<CR>", "Location List"},
            q = {"<cmd>call PMToggleView('quickfix')<CR>", "QuickFix List"},
            s = {"<cmd>call PMToggleView('symbols')<CR>", "Symbol List"},
            x = {"<cmd>call PMToggleView('nvim-tree')<CR>", "File Tree"},
            T = {"<cmd>call PMToggleView('term')<CR>", "All Terminals"},
            t = {"<cmd>1ToggleTerm<cr>", "Terminal"},
            u = {"<cmd>call PMToggleView('undotree')<CR>", "Undo Tree"},
            v = {"<cmd>call CloseAllPanels()<cr>", "Close All Panels"},
        },
        Q = {"<cmd>CClear<cr><cmd>cgetbuffer<cr><cmd>TroubleRefresh<cr>", "Populater QF List With Buffer Errors "},
        q = {
            name = "QuickFix List",
            a = {"<cmd>caddbuffer<cr><cmd>TroubleRefresh<cr>", "Add Buffer Errrors to QF List"},
            c = {"<cmd>CClear<cr><cmd>TroubleRefresh<cr>", "Clear The List"},
            g = {"<cmd>Git! difftool<cr><cmd>Trouble quickfix<cr>", "Populate With Diffs"},
            n = {"<cmd>cnewer<cr>", "Newer List"},
            p = {"<cmd>colder<cr>", "Older List"},
            q = {"<cmd>call PMToggleView('quickfix')<CR>", "Open"},
            f = {"<cmd>call PMToggleView('quickfixFilter')<CR>", "Filter List"},
            V = {[["<cmd> noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj **/* <cr><cmd>Trouble quickfix<cr>"]], "Populate With VimGrep (file select)", expr=true},
            v = {[["<cmd> noautocmd vimgrep /" . input("What would you like to vimgrep? > ") . "/gj " . input("In what files? > ") . "<cr><cmd>Trouble quickfix<cr>"]], "Populate With VimGrep", expr=true},
            w = {[["<cmd> Cfind! " . input("What would you like to find? ")  . "<cr><cmd>Trouble quickfix<cr>"]], "Populate With find", expr=true},
            W = {[["<cmd> Clocate! " . input("What would you like to locate? ")  . "<cr><cmd>Trouble quickfix<cr>"]], "Populate With Locate", expr=true},
            -- s = {"<cmd>SpellCheck!<cr>", "Populate With Spelling Errors"},
        },
        L = {"<cmd>LClear<cr><cmd>lgetbuffer<cr><cmd>TroubleRefresh<cr>", "Populater LocList With Buffer Errors "},
        l = {
            name = "Location List",
            a = {"<cmd>laddbuffer<cr><cmd>TroubleRefresh<cr>", "Add Buffer Errrors to LocList"},
            c = {"<cmd>LClear<cr><cmd>TroubleRefresh<cr>", "Clear The List"},
            l = {"<cmd>call PMToggleView('loclist')<CR>", "Open Location List"},
            f = {"<cmd>call PMToggleView('loclistFilter')<CR>", "Filter List"},
            n = {"<cmd>lnewer<cr>", "Newer List"},
            p = {"<cmd>lolder<cr>", "Older List"},
            V = {[["<cmd> noautocmd lvimgrep /" . input("What would you like to vimgrep? > ") . "/gj **/* <cr><cmd>Trouble loclist<cr>"]], "Populate With VimGrep (file select)", expr=true},
            v = {[["<cmd> noautocmd lvimgrep /" . input("What would you like to vimgrep? > ") . "/gj " . input("In what files? > ") . "<cr><cmd>Trouble loclist<cr>"]], "Populate With VimGrep", expr=true},
            w = {[["<cmd> Lfind! " . input("What would you like to find? ")  . "<cr><cmd>Trouble loclist<cr>"]], "Populate With find", expr=true},
            W = {[["<cmd> Llocate! " . input("What would you like to locate? ")  . "<cr><cmd>Trouble loclist<cr>"]], "Populate With Locate", expr=true},
            -- s = {"<cmd>SpellLCheck!<cr>", "Populate With Spelling Errors"},
        },
        E = {"<cmd>CClear<cr><cmd>cgetbuffer<cr><cmd>TroubleRefresh<cr>", "Open Buffre Errors in Touble"},
        e = {
            name = "Errors",
            e = {"<cmd>call PMToggleView('errorlist')<CR>", "Open Errors"},
            E = {"<cmd>call PMToggleView('errorlistdoc')<CR>", "Open Errors (buffer)"},
            n = {"<cmd>call PMToggleView('Todo-Trouble')<cr>", "Todo Items"},
            f = {"<cmd>call PMToggleView('troubleTelescope')<CR>", "Open Telescope List"},
            r = {"<cmd>TroubleRefresh<cr>", "Refresh Errors"},
        },
        t = {
            name = "Terminal",
            t = {"<cmd>1ToggleTerm<cr>", "Open Terminal"},
            q = {[[<cmd>1TermExec cmd="exit"<CR>]], "Exit Terminal"},
            Q = {[[<cmd>1TermExec cmd="exit"<CR><cmd>2TermExec cmd="exit"<CR><cmd>3TermExec cmd="exit"<cr>]], "Exit All Terminals"},
            i = {"<cmd>1ToggleTerm<cr><cmd>2ToggleTerm<cr><cmd>3ToggleTerm<cr><cmd>ToggleTermCloseAll<cr>", "Initialize Terminals"}
        },
        i = {
            name = "REPL Terminal",
            i = {"<cmd>3ToggleTerm<CR>", "Open REPL"},
            q = {[[<cmd>3TermExec cmd="exit"<CR>]], "Exit REPL"},
        },
        m = {
            name = "Make Terminal",
            q = {[[<cmd>2TermExec cmd="exit"<cr>]], "Exit Make Terminal"},
        },
        r = {
            name = "Refactor",
            t = {"Rename (Treesitter)"},
            v = {"<plug>(ExtractVar)", "Extract Variable"},
        },
        x = {
            name = "Explorer",
            t = {"<cmd>call PMToggleView('nvim-tree')<cr>", "Open Explorer"},
        },
        b = {
            name = "buffers",
            o = {"<cmd>Bdelete hidden<cr>", "Close All Hidden Buffers"},
            d = {"<cmd>bdelete!<cr>", "Delete the current buffer"},
            c = {"<cmd>q<cr>", "Close the current buffer"},
        },
        w = {
            name = "Window Managment",
            w = {"<cmd>ZenMode<cr>", "Zen Mode"},
            o = {"<c-w>o", "Clean Up Windows"},
            ["<bs>"] = {"<c-w>c", "Close Window"},
            ["<cr>"] = {"<c-w>v", "Open Window"},
            x = {"<c-w>s", "Horizontal Split"},
            v = {"<c-w>v", "Vertical Split"},
            n = {"<C-W>w", "Next Window"},
            p = {"<C-W>W", "Previous Window"},
            N = {"<C-W>r", "Move Window Next"},
            P = {"<C-W>R", "Move Window Previous"},
            ["]"] = {"<cmd>vertical resize +5<cr>", "Vertical Resize"},
            ["["] = {"<cmd>vertical resize -5<cr>", "Vertical Resize"},
            ["}"] = {"<cmd>resize +5<cr>", "Horizontal Resize"},
            ["{"] = {"<cmd>resize -5<cr>", "Horizontal Resize"},
            ["="] = {"<c-w>=", "Equal Size"},
            h = {"<c-w>h", "Left Windown"},
            j = {"<c-w>j", "Below Window"},
            k = {"<c-w>k", "Above Window"},
            l = {"<c-w>l", "Right Window"},
            ["<left>"] = {"<c-w>h", "Left Windown"},
            ["<down>"] = {"<c-w>j", "Below Window"},
            ["<up>"] = {"<c-w>k", "Above Window"},
            ["<right>"] = {"<c-w>l", "Right Window"},
            H = {"<c-w>H", "Move Far Left"},
            J = {"<c-w>J", "Move Far Down"},
            K = {"<c-w>K", "Move Far Up"},
            L = {"<c-w>L", "Move Far Right"},
            c = {"<c-w>c", "Close Window"},
            ["/"] = {"<c-w>^", "Open Alternate File"},
            [","] = {"<cmd>BufferLineCyclePrev<cr>", "Previous Buffer"},
            ["."] = {"<cmd>BufferLineCycleNext<cr>", "Next Buffer"},
        },
        [","] = {
            name = "Settings",
            [","] = {"<cmd>Telescope vim_options<cr>", "Vim Options"},
            s = {"<cmd>set spell!<cr>", "Toggle Spelling"},
            k = {"<cmd>Telescope keymaps<cr>", "Keymaps"},
            c = {"<cmd>Telescope colorscheme<cr>", "Color Schemes"},
            C = {"<cmd>Telescope highlights<cr>", "Highlight Groups"},
            a = {"<cmd>Telescope autocommands<cr>", "AutoCommands"},
            f = {"<cmd>Telescope filetypes<cr>", "FileTypes"},
            h = {"<cmd>Telescope help_tags<cr>", "Help Tags"},
            m = {"<cmd>Telescope man_pages<cr>", "Man Pages"},
        },
        h = {
            h = {"<cmd>Telescope help_tags<cr>", "Search Help Tags"},
            i = {"<cmd>LvimHelper<cr>", "Insert Mode Mappings"},
            k = {"K", "Documentation"},
        }
    },
    ["["] = {
        name = "Backward Leader",
        L = {"<cmd>try <bar> lpfile <bar> catch /E553/ <bar> llast <bar> endtry<CR>", "Loclist File"},
        Q = {"<cmd>try <bar> cpfile <bar> catch /E553/ <bar> clast <bar> endtry<CR>", "QuickFix File"},
        l = {"<cmd>try <bar> lprevious <bar> catch /E553/ <bar> llast <bar> endtry<CR>", "LocList Entry"},
        q = {"<cmd>try <bar> cprevious <bar> catch /E553/ <bar> clast <bar> endtry<CR>", "QuickFix Entry"},
        t = {"<cmd>tabprevious<cr>", "Tab"},
        h = {"Hunk"},
        s = {"[s", "Spelling Mistake"},
        ["["] = {"Section"},
        ["]"] = {"Section (end)"},
    },
    ["]"] = {
        name = "Forward Leader",
        L = {"<cmd>try <bar> lnfile <bar> catch /E553/ <bar> lfirst <bar> endtry<CR>", "LocList File"},
        Q = {"<cmd>try <bar> cnfile <bar> catch /E553/ <bar> cfirst <bar> endtry<CR>", "QuickFix File"},
        l = {"<cmd>try <bar> lnext <bar> catch /E553/ <bar> lfirst <bar> endtry<CR>", "LocList Entry"},
        q = {"<cmd>try <bar> cnext <bar> catch /E553/ <bar> cfirst <bar> endtry<CR>", "QuickFix Entry"},
        t = {"<cmd>tabnext<cr>", "Tab"},
        h = {"Hunk"},
        s = {"]s", "Spelling Mistake"},
        ["["] = {"Section (end)"},
        ["]"] = {"Section"},
    },
    ["<localleader>"] = {
        name = "Local Leader",
    },
})
require("which-key").register({
    ["<leader>"] = {
        r = {
            name = "Refactor",
            v = {"<plug>(ExtractVarVis)", "Extract Variable"},
        },
    },
    g = {
        R = {"<plug>(SubversiveSubstituteToEndOfLine)", "Substitute to EOL"},
        r = {"<plug>(SubversiveSubstitute)", "Substitute"},
        rr = {"<plug>(SubversiveSubstituteLine)", "Substitute Line"},
        rR = {"<plug>(SubversiveSubstitute)H", "Substitute to SOL"},
        j = {"J", "Join"},
        k = {"c<cr><esc>", "Split"},
        t = {"<Plug>(EasyAlign)", "Align"},
        s = {
            name = "Change Case",
            p = {"<Plug>CaserVMixedCase", "Pascal Case"},
            c = {"<Plug>CaserVCamelCase", "Camel Case"},
            ["_"] = {"<Plug>CaserVSnakeCase", "Snake Case"},
            u = {"<Plug>CaserVUpperCase", "Upper Case"},
            t = {"<Plug>CaserVTitleCase", "Title Case"},
            s = {"<Plug>CaserVSentenceCase", "Sentance Case"},
            ["<space>"] = {"<Plug>CaserVSpaceCase", "Space Case"},
            ["-"] = {"<Plug>CaserVKebabCase", "Kebab Case"},
            k = {"<Plug>CaserVTitleKebabCase", "Title Case"},
            ["."] = {"<Plug>CaserVDotCase", "Dot Case"},
        },
    },
    z = {
        i = {"I", "Insert"},
        a = {"A", "Append"},
    },
},{ mode = "v" })

