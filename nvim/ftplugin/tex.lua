-- vim.b[0].localCommands = {
--     { source = "tex", name = "Build project",    command = "silent VimtexCompileSS" },
--     { source = "tex", name = "Table of content", command = "VimtexTocToggle" },
--     { source = "tex", name = "Forward Search",   command = "TexlabForward" },
--     { source = "tex", name = "Word Count",       command = "VimtexCountWord" },
-- }

Map("n", "<localleader><localleader>", function()
    vim.cmd("TexlabForward")
    -- if tonumber(vim.fn.systemlist([[xrandr | grep \* | rg -o -e '\d+' | head -n1]])[1]) > 1920 then
        vim.cmd("sleep 20m")
        vim.cmd("silent !xdotool key Escape")
        vim.cmd("sleep 200m")
        vim.cmd("silent !xdotool key super+n")
    -- end
end, { buffer = 0, silent = true })

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
vim.g.vimtex_view_general_viewer = "zathura --synctex-editor-command='nvr --servername " ..
vim.v.servername .. " +%{line} %{input}'"
vim.g.vimtex_view_forward_search_on_start = 1
vim.g.vimtex_view_automatic = 0
vim.g.vimtex_compiler_latexmk = {
    ["callback"] = 1,
    ["continuous"] = 0,
    ["executable"] = "latexmk",
    ["hooks"] = {},
    ["options"] = { "-pdf", "-verbose", "-file-line-error", "-synctex=1", "-interaction=nonstopmode" },
}

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
