-- Visual Bindings
require("which-key").register({
    j = { [[v:count?(v:count>5?"m'".v:count:'').'j':'gj']], "down", expr = true },
    k = { [[v:count?(v:count>5?"m'".v:count:'').'k':'gk']], "up", expr = true },
    H = { [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], "Start of Line", expr = true },
    L = { [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], "End of Line", expr = true },
    Q = { "@q", "Play The Temp Macro" },
    -- ["<c-j>"] = {"H", "Top of Window"},
    -- ["<c-h>"] = {"M", "Top of Window"},
    -- ["<c-k>"] = {"L", "Top of Window"},
    ["<"] = { "<gv", "Dedent" },
    [">"] = { ">gv", "Indent" },
    ["J"] = { ":move '>+1<cr>gv=gv", "Move Line Down" },
    ["K"] = { ":move '<-2<cr>gv=gv", "Move Line Up" },
    ["S"] = { ":lua require('tsht').nodes()<CR>", "Move Line Up" },
    s = {
        name = "Select Mode",
        s = { "<cmd>lua require'hop'.hint_char1()<cr>", "Hop Char" },
        n = { "v[a", "Left Outside", noremap=false},
        e = { "v[i", "Left Inside", noremap=false},
        i = { "v]i", "Right Inside", noremap=false},
        o = { "v]a", "Right Outside", noremap=false},
        l = { "vi", "Inside", noremap=false},
        u = { "va", "Outside", noremap=false},
    },
    ["'"] = { "`", "Jump to mark location" },
    ["`"] = { "'", "Jump to mark line" },
    ["<leader>"] = {
        ["."] = { "<cmd>Telescope lsp_range_code_actions theme=get_cursor<CR>", "Code Actions" },
        r = {
            name = "Refactor",
            v = { "<plug>(ExtractVarVis)", "Extract Variable" },
            ["="] = { "<cmd>lua vim.lsp.buf.range_formatting()<CR>", "Format" },
        },
        g = {
            s = { "<cmd>lua require'gitsigns'.stage_hunk({vim.fn.line('.'), vim.fn.line('.')})", "Stage Hunks in Range" },
            r = { "<cmd>lua require'gitsigns'.reset_hunk({vim.fn.line('.'), vim.fn.line('.')})", "Reset Hunks in Range" },
        },
        z = {
            w = { [["!par w" . &textwidth . "<cr>"]], "Wrap to Textwidth", expr=true },
            d = { [[:%s/\v[^^ ]\zs  / /g<cr>]], "Remove Double Spaces" },
        },
        j = {
            j = { ":<c-u>MagmaEvaluateVisual<cr>", "Evaluate Selction" },
        },
    },
    ["g,"] = {
        name = "User Commands",
        [";"] = { "q:", "Command Buffer" },
        R = { "<plug>(SubversiveSubstituteToEndOfLine)", "Substitute to EOL" },
        r = { "<plug>(SubversiveSubstitute)", "Substitute" },
        rr = { "<plug>(SubversiveSubstituteLine)", "Substitute Line" },
        rR = { "<plug>(SubversiveSubstitute)H", "Substitute to SOL" },
        j = { "J", "Join" },
        k = { "c<cr><esc>", "Split" },
        t = { "<Plug>(EasyAlign)", "Align" },
        h = { "gv<cmd>lua require'surround'.surround_add()<cr>", "Hug" },
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
        z = { [["!par w" . &textwidth . "<cr>"]], "Wrap to Textwidth", expr=true },
    },
    v = {
        name = "View",
        v = { "zz", "Centre Cursor (Vertically)" },
        m = { "<cmd>set sidescrolloff=999<cr><cmd>set sidescrolloff=0<cr>", "Centre Cursor (Horizontally)" },
        b = { "zb", "Cursor On Bottom" },
        t = { "zt", "Cursor On Top" },
        e = { "ze", "Cursor At Right" },
        s = { "zs", "Cursor At Left" },
        h = { "zh", "Scroll Left" },
        l = { "zl", "Scroll Right" },
        j = { "<c-e>", "Scroll Down" },
        k = { "<c-y>", "Scroll Up" },
        o = { "za", "Open Fold" },
        c = { "zc", "Close Fold" },
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
        l = { ":<c-u>TSTextobjectSelect @loop.outer<cr>", "Loop" },
        o = { ":<c-u>TSTextobjectSelect @class.outer<cr>", "Class" },
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
        N = {
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
        l = { ":<c-u>TSTextobjectSelect @loop.inner<cr>", "Loop" },
        o = { ":<c-u>TSTextobjectSelect @class.inner<cr>", "Class" },
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
        N = {
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
}, {
    mode = "x",
})
