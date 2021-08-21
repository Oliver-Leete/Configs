-- Visual Bindings
require("which-key").register({
    j = {[[v:count?(v:count>5?"m'".v:count:'').'j':'gj']], "down", expr=true},
    k = {[[v:count?(v:count>5?"m'".v:count:'').'k':'gk']], "up", expr=true},
    H = {[[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], "Start of Line", expr=true},
    L = {[[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], "End of Line", expr=true},
    ["<leader>"] = {
        ["."] = { "<cmd>Telescope lsp_range_code_actions theme=get_cursor<CR>", "Code Actions" },
        r = {
            name = "Refactor",
            v = { "<plug>(ExtractVarVis)", "Extract Variable" },
            ["="] = { "<cmd>lua vim.lsp.buf.range_formatting()<CR>", "Format" },
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
}, {
    mode = "x",
})
