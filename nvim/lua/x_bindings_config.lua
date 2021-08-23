-- Visual Bindings
require("which-key").register({
    j = {[[v:count?(v:count>5?"m'".v:count:'').'j':'gj']], "down", expr=true},
    k = {[[v:count?(v:count>5?"m'".v:count:'').'k':'gk']], "up", expr=true},
    H = {[[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], "Start of Line", expr=true},
    L = {[[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], "End of Line", expr=true},
    ["<c-j>"] = {"H", "Top of Window"},
    ["<c-h>"] = {"M", "Top of Window"},
    ["<c-k>"] = {"L", "Top of Window"},
    [";"] = {":", "Command Mode"},
    [":"] = {"<nop>", "Nothing at the moment"},
    ["q;"] = {"q:", "Ex Mode"},
    ["@;"] = {"@:", "Command Register"},
    ["<"] = {"<gv", "Dedent"},
    [">"] = {">gv", "Indent"},
    ["J"] = {":move '>+1<cr>gv=gv", "Move Line Down"},
    ["K"] = {":move '<-2<cr>gv=gv", "Move Line Up"},
    ["S"] = {":lua require('tsht').nodes()<CR>", "Move Line Up"},
    ["<leader>"] = {
        ["."] = { "<cmd>Telescope lsp_range_code_actions theme=get_cursor<CR>", "Code Actions" },
        r = {
            name = "Refactor",
            v = { "<plug>(ExtractVarVis)", "Extract Variable" },
            ["="] = { "<cmd>lua vim.lsp.buf.range_formatting()<CR>", "Format" },
        },
        g = {
            s = {"<cmd>lua require'gitsigns'.stage_hunk({vim.fn.line('.'), vim.fn.line('.')})", "Stage Hunks in Range"},
            r = {"<cmd>lua require'gitsigns'.reset_hunk({vim.fn.line('.'), vim.fn.line('.')})", "Reset Hunks in Range"},
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
        z = { "!par w80<cr>", "Wrap to 80 Characters" },
    },
    z = {
        i = { "I", "Insert" },
        a = { "A", "Append" },
    },
    a = {
        name = "around",
        h = {":<c-u>Gitsigns selct_hunk<cr>", "Git Hunk"},
        B = {":<c-u>TSTextobjectSelect @block.outer<cr>", "Block"},
        c = {":<c-u>TSTextobjectSelect @conditional.outer<cr>", "Conditional"},
        [","] = {":<c-u>TSTextobjectSelect @parameter.outer<cr>", "Parameter"},
        d = {":<c-u>TSTextobjectSelect @comment.outer<cr>", "Comment"},
        f = {":<c-u>TSTextobjectSelect @function.outer<cr>", "Function"},
        F = {":<c-u>TSTextobjectSelect @call.outer<cr>", "Function"},
        l = {":<c-u>TSTextobjectSelect @loop.outer<cr>", "Loop"},
        o = {":<c-u>TSTextobjectSelect @class.outer<cr>", "Class"},
        n = {
            name = "Next",
            h = {":<c-u>call v:lua.git_target(v:count, 'true')<cr>", "Git Hunk"},
            B = {":<c-u>call v:lua.ts_target(v:count, '@block.outer')<cr>", "Block"},
            c = {":<c-u>call v:lua.ts_target(v:count, '@conditional.outer')<cr>", "Conditional"},
            [","] = {":<c-u>call v:lua.ts_target(v:count, '@parameter.outer')<cr>", "Parameter"},
            d = {":<c-u>call v:lua.ts_target(v:count, '@comment.outer')<cr>", "Comment"},
            f = {":<c-u>call v:lua.ts_target(v:count, '@function.outer')<cr>", "Function"},
            F = {":<c-u>call v:lua.ts_target(v:count, '@call.outer')<cr>", "Function"},
            l = {":<c-u>call v:lua.ts_target(v:count, '@loop.outer')<cr>", "Loop"},
            o = {":<c-u>call v:lua.ts_target(v:count, '@class.outer')<cr>", "Class"},
        },
        N = {
            name = "Previous",
            h = {":<c-u>call v:lua.git_target(v:count, 'false')<cr>", "Git Hunk"},
            B = {":<c-u>call v:lua.ts_target_back(v:count, '@block.outer')<cr>", "Block"},
            c = {":<c-u>call v:lua.ts_target_back(v:count, '@conditional.outer')<cr>", "Conditional"},
            [","] = {":<c-u>call v:lua.ts_target_back(v:count, '@parameter.outer')<cr>", "Parameter"},
            d = {":<c-u>call v:lua.ts_target_back(v:count, '@comment.outer')<cr>", "Comment"},
            f = {":<c-u>call v:lua.ts_target_back(v:count, '@function.outer')<cr>", "Function"},
            F = {":<c-u>call v:lua.ts_target_back(v:count, '@call.outer')<cr>", "Function"},
            l = {":<c-u>call v:lua.ts_target_back(v:count, '@loop.outer')<cr>", "Loop"},
            o = {":<c-u>call v:lua.ts_target_back(v:count, '@class.outer')<cr>", "Class"},
        },
    },
    i = {
        name = "inside",
        h = {":<c-u>Gitsigns selct_hunk<cr>", "Git Hunk"},
        B = {":<c-u>TSTextobjectSelect @block.inner<cr>", "Block"},
        c = {":<c-u>TSTextobjectSelect @conditional.inner<cr>", "Conditional"},
        [","] = {":<c-u>TSTextobjectSelect @parameter.inner<cr>", "Parameter"},
        d = {":<c-u>TSTextobjectSelect @comment.outer<cr>", "Comment"},
        f = {":<c-u>TSTextobjectSelect @function.inner<cr>", "Function"},
        F = {":<c-u>TSTextobjectSelect @call.inner<cr>", "Function"},
        l = {":<c-u>TSTextobjectSelect @loop.inner<cr>", "Loop"},
        o = {":<c-u>TSTextobjectSelect @class.inner<cr>", "Class"},
        n = {
            name = "Next",
            h = {":<c-u>call v:lua.git_target(v:count, 'true')<cr>", "Git Hunk"},
            B = {":<c-u>call v:lua.ts_target(v:count, '@block.inner')<cr>", "Block"},
            c = {":<c-u>call v:lua.ts_target(v:count, '@conditional.inner')<cr>", "Conditional"},
            [","] = {":<c-u>call v:lua.ts_target(v:count, '@parameter.inner')<cr>", "Parameter"},
            d = {":<c-u>call v:lua.ts_target(v:count, '@comment.outer')<cr>", "Comment"},
            f = {":<c-u>call v:lua.ts_target(v:count, '@function.inner')<cr>", "Function"},
            F = {":<c-u>call v:lua.ts_target(v:count, '@call.inner')<cr>", "Function"},
            l = {":<c-u>call v:lua.ts_target(v:count, '@loop.inner')<cr>", "Loop"},
            o = {":<c-u>call v:lua.ts_target(v:count, '@class.inner')<cr>", "Class"},
        },
        N = {
            name = "Previous",
            h = {":<c-u>call v:lua.git_target(v:count, 'false')<cr>", "Git Hunk"},
            B = {":<c-u>call v:lua.ts_target_back(v:count, '@block.inner')<cr>", "Block"},
            c = {":<c-u>call v:lua.ts_target_back(v:count, '@conditional.inner')<cr>", "Conditional"},
            [","] = {":<c-u>call v:lua.ts_target_back(v:count, '@parameter.inner')<cr>", "Parameter"},
            d = {":<c-u>call v:lua.ts_target_back(v:count, '@comment.outer')<cr>", "Comment"},
            f = {":<c-u>call v:lua.ts_target_back(v:count, '@function.inner')<cr>", "Function"},
            F = {":<c-u>call v:lua.ts_target_back(v:count, '@call.inner')<cr>", "Function"},
            l = {":<c-u>call v:lua.ts_target_back(v:count, '@loop.inner')<cr>", "Loop"},
            o = {":<c-u>call v:lua.ts_target_back(v:count, '@class.inner')<cr>", "Class"},
        },
    },
}, {
    mode = "x",
})
