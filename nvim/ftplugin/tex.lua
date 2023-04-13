vim.b[0].localCommands = {
    { source = "tex", name = "Build project",    command = "silent VimtexCompileSS" },
    { source = "tex", name = "Table of content", command = "VimtexTocToggle" },
    { source = "tex", name = "Forward Search",   command = "TexlabForward" },
    { source = "tex", name = "Word Count",       command = "VimtexCountWord" },
}

Map("n", "<localleader><localleader>", "<cmd>TexlabForward<cr>", { buffer = 0, silent = true })

Map("n", "KK", "<cmd>VimtexDocPackage<cr>", { buffer = 0 })

Map({ "x", "o" }, "am", "<plug>(vimtex-a$)", { buffer = 0, remap = true })
Map({ "x", "o" }, "im", "<plug>(vimtex-i$)", { buffer = 0, remap = true })
Map({ "n", "x", "o" }, "[m", "<cmd>let g:dirJumps='n'<cr>m`<plug>(vimtex-[n)zz", { buffer = 0 })
Map({ "x", "o" }, "alm", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-[l)', '(vimtex-a$)')<cr>", { buffer = 0 })
Map({ "x", "o" }, "ilm", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-[l)', '(vimtex-i$)')<cr>", { buffer = 0 })
Map({ "n", "x", "o" }, "]m", "<cmd>let g:dirJumps='n'<cr>m`<plug>(vimtex-]n)zz", { buffer = 0 })
Map({ "x", "o" }, "anm", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]n)', '(vimtex-a$)')<cr>", { buffer = 0 })
Map({ "x", "o" }, "inm", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]n)', '(vimtex-i$)')<cr>", { buffer = 0 })

Map("n", "dpe", "<plug>(vimtex-env-delete)", { buffer = 0, remap = true })
Map("n", "dpc", "<plug>(vimtex-cmd-delete)", { buffer = 0, remap = true })
Map("n", "dpd", "<plug>(vimtex-delim-delete)", { buffer = 0, remap = true })
Map("n", "cpe", "<plug>(vimtex-env-change)", { buffer = 0, remap = true })
Map("n", "cpc", "<plug>(vimtex-cmd-change)", { buffer = 0, remap = true })
Map("n", "cpd", "<plug>(vimtex-delim-change)", { buffer = 0, remap = true })

Map("n", "ype", "<plug>(vimtex-env-surround-operator)", { buffer = 0, remap = true })
Map("x", "ype", "<plug>(vimtex-env-surround-visual)", { buffer = 0, remap = true })

Map("n", "<localleader>d", "<plug>(vimtex-delim-add-modifiers)", { buffer = 0, remap = true })
Map("n", "%", "<plug>(vimtex-%)", { buffer = 0, remap = true })

-- vim.keymap.del("n", ",n", { buffer = 0 })
Map("n", ",nm", "<plug>(vimtex-env-toggle-math)", { buffer = 0, remap = true })
Map("n", ",nf", "<plug>(vimtex-cmd-toggle-frac)", { buffer = 0, remap = true })
Map("n", ",nd", "<plug>(vimtex-delim-toggle-modifier)", { buffer = 0, remap = true })

vim.api.nvim_buf_set_option(0, "textwidth", 100)

vim.g.tex_flavor = "latex"
vim.g.vimtex_quickfix_mode = 0
vim.g.vimtex_doc_confirm_single = 0
vim.g.vimtex_view_general_viewer = "zathura --synctex-editor-command='nvr --servername " .. vim.v.servername .. " +%{line} %{input}'"
vim.g.vimtex_view_forward_search_on_start = 1
vim.g.vimtex_view_automatic = 0
vim.g.vimtex_compiler_latexmk = {
    ["callback"] = 1,
    ["continuous"] = 0,
    ["executable"] = "latexmk",
    ["hooks"] = {},
    ["options"] = { "-pdf", "-verbose", "-file-line-error", "-synctex=1", "-interaction=nonstopmode" },
}

local conceal_table = {
    {
        name = "unit",
        conceal = true,
    },
    {
        name = "num",
        conceal = true,
    },
    {
        name = "numproduct",
        conceal = true,
    },
    {
        name = "numlist",
        conceal = true,
    },
    {
        name = "ch",
        conceal = true,
        argstyle = "bold",
    },
}

for _, cmd in pairs({
    { name = "per",     symbol = "/" },
    { name = "percent", symbol = "%" },
    { name = "ampere",  symbol = "A" },
    -- { name = "candela",   symbol = "cd" },
    { name = "kelvin",  symbol = "K" },
    -- { name = "kilogram",  symbol = "kg" },
    { name = "gram",    symbol = "g" },
    { name = "metre",   symbol = "m" },
    { name = "meter",   symbol = "m" },
    -- { name = "mole",      symbol = "mol" },
    { name = "second",  symbol = "s" },
    { name = "minute",  symbol = "m" },
    { name = "hour",    symbol = "h" },
    { name = "newton",  symbol = "N" },
    { name = "celsius", symbol = "C" },
    { name = "ohm",     symbol = "Ω" },
    -- { name = "coulomb",   symbol = "C" },
    -- { name = "pascal",    symbol = "Pa" },
    { name = "farad",   symbol = "F" },
    -- { name = "radian",    symbol = "rad" },
    -- { name = "gray",      symbol = "Gy" },
    { name = "siemens", symbol = "S" },
    -- { name = "hertz",     symbol = "Hz" },
    -- { name = "sievert",   symbol = "Sv" },
    { name = "henry",   symbol = "H" },
    -- { name = "steradian", symbol = "sr" },
    { name = "joule",   symbol = "J" },
    { name = "tesla",   symbol = "T" },
    -- { name = "lumen",     symbol = "lm" },
    { name = "volt",    symbol = "V" },
    -- { name = "katal",     symbol = "kat" },
    { name = "watt",    symbol = "W" },
    -- { name = "lux",       symbol = "lx" },
    -- { name = "weber",     symbol = "Wb" },
    { name = "quecto",  symbol = "q" },
    { name = "ronto",   symbol = "r" },
    { name = "yocto",   symbol = "y" },
    { name = "atto",    symbol = "a" },
    { name = "zepto",   symbol = "z" },
    { name = "femto",   symbol = "f" },
    { name = "pico",    symbol = "p" },
    { name = "nano",    symbol = "n" },
    { name = "micro",   symbol = "µ" },
    { name = "milli",   symbol = "m" },
    { name = "centi",   symbol = "c" },
    { name = "deci",    symbol = "d" },
    { name = "hecto",   symbol = "h" },
    { name = "kilo",    symbol = "k" },
    { name = "mega",    symbol = "M" },
    { name = "giga",    symbol = "G" },
    { name = "tera",    symbol = "T" },
    { name = "peta",    symbol = "P" },
    { name = "exa",     symbol = "E" },
    { name = "zetta",   symbol = "Z" },
    { name = "yotta",   symbol = "Y" },
    { name = "ronna",   symbol = "R" },
    { name = "quetta",  symbol = "Q" },
}) do
    table.insert(conceal_table, {
        name = cmd.name,
        conceal = true,
        concealchar = cmd.symbol,
        arg = false,
    })
    table.insert(conceal_table, {
        name = cmd.name,
        conceal = true,
        concealchar = cmd.symbol,
        arg = false,
        mathmode = true,
    })
end

for _, cmdname in pairs({ "ac", "acf", "acs", "acl", "pac", "Ac", "Acf", "Acs", "Acl", "Pac" }) do
    table.insert(conceal_table, {
        name = cmdname,
        conceal = true,
        argstyle = "bold",
    })
    table.insert(conceal_table, {
        name = cmdname,
        conceal = true,
        argstyle = "bold",
        mathmode = true,
    })
end

-- vim.g.vimtex_syntax_custom_cmds = conceal_table

local conceal_pairs_table = {
    {
        name = "qty",
        conceal = true,
        nargs = 2,
        cchar_mid = " ",
    },
    {
        name = "qtyproduct",
        conceal = true,
        nargs = 2,
        cchar_mid = " ",
    },
    {
        name = "qtyrange",
        conceal = true,
        nargs = 3,
        cchar_mid = "-",
    },
    {
        name = "qtylist",
        conceal = true,
        nargs = 2,
        cchar_mid = " ",
    },
    {
        name = "numrange",
        conceal = true,
        nargs = 2,
        cchar_mid = "-",
    },
    {
        name = "qty",
        conceal = true,
        nargs = 2,
        cchar_mid = " ",
        mathmode = true,
    },
    {
        name = "qtyproduct",
        conceal = true,
        nargs = 2,
        cchar_mid = " ",
        mathmode = true,
    },
    {
        name = "qtyrange",
        conceal = true,
        nargs = 3,
        cchar_mid = "-",
        mathmode = true,
    },
    {
        name = "qtylist",
        conceal = true,
        nargs = 2,
        cchar_mid = " ",
        mathmode = true,
    },
    {
        name = "numrange",
        conceal = true,
        nargs = 2,
        cchar_mid = "-",
        mathmode = true,
    },
    {
        name = "coord",
        conceal = true,
        nargs = 1,
        cchar_open = "(",
        cchar_close = ")",
    },
    {
        name = "Int",
        conceal = true,
        nargs = 1,
        cchar_open = "#",
        mathmode = true,
    },
    {
        name = "Call",
        conceal = true,
        nargs = 2,
        argstyle = "bold",
        cchar_mid = "(",
        cchar_close = ")",
    },
    {
        name = "Call",
        conceal = true,
        nargs = 2,
        argstyle = "bold",
        cchar_mid = "(",
        cchar_close = ")",
        mathmode = true,
    },
}
-- vim.g.vimtex_syntax_custom_cmds_with_concealed_delims = conceal_pairs_table

-- vim.opt_local.conceallevel = 2
-- vim.g.vimtex_syntax_conceal = {
--     ["accents"] = true,
--     ["ligatures"] = true,
--     ["cites"] = true,
--     ["fancy"] = true,
--     ["greek"] = true,
--     ["spacing"] = true,
--     ["math_bounds"] = true,
--     ["math_delimiters"] = true,
--     ["math_fracs"] = true,
--     ["math_super_sub"] = false,
--     ["math_symbols"] = true,
--     ["sections"] = true,
--     ["styles"] = true,
-- }
vim.g.vimtex_syntax_conceal = {
    ["accents"] = false,
    ["ligatures"] = false,
    ["cites"] = false,
    ["fancy"] = false,
    ["greek"] = false,
    ["spacing"] = false,
    ["math_bounds"] = false,
    ["math_delimiters"] = false,
    ["math_fracs"] = false,
    ["math_super_sub"] = true,
    ["math_symbols"] = false,
    ["sections"] = false,
    ["styles"] = false,
}

vim.g.projectionist_heuristics = {
    ["OML-Thesis.tex"] = {
        ["1.tex"] = {
            type = "chapter",
            alternate = "OML-Thesis.tex",
        },
        ["2.tex"] = {
            type = "chapter",
            alternate = "OML-Thesis.tex",
        },
        ["3.tex"] = {
            type = "chapter",
            alternate = "OML-Thesis.tex",
        },
        ["4.tex"] = {
            type = "chapter",
            alternate = "OML-Thesis.tex",
        },
        ["5.tex"] = {
            type = "chapter",
            alternate = "OML-Thesis.tex",
        },
        ["6.tex"] = {
            type = "chapter",
            alternate = "OML-Thesis.tex",
        },
        ["7.tex"] = {
            type = "chapter",
            alternate = "OML-Thesis.tex",
        },
        ["Appendix.tex"] = {
            type = "chapter",
            alternate = "OML-Thesis.tex",
        },
        ["*.tex"] = {
            type = "section",
            alternate = "{dirname}.tex",
        },
        ["Scripts/*.tex"] = {
            type = "scripts",
            alternate = "OML-Thesis.tex",
        },
        ["Scripts/Packages.tex"] = { type = "deps" },
        ["IntroductiontoAM.tex"] = { type = "chapOneMain" },
        ["PowderBedFusion.tex"] = { type = "chapTwoMain" },
        ["ModellingofAM.tex"] = { type = "chapThreeMain" },
        ["PowderCharacterisation.tex"] = { type = "chapFourMain" },
        ["PowderModel.tex"] = { type = "chapFiveMain" },
        ["MachineModel.tex"] = { type = "chapSixMain" },
        ["ProcessModel.tex"] = { type = "chapSevenMain" },
        ["OML-Thesis.tex"] = { type = "main" },
        ["Citations.bib"] = { type = "citations" },
    },
}

if vim.g.viewerOpen ~= 1 then
    vim.g.viewerOpen = 1
    vim.cmd("VimtexView")
    vim.cmd("sleep 200m")
    vim.cmd("silent !xdotool key super+n")
end

vim.go.winbar = "%{%v:lua.Normal_Winbar()%}"
