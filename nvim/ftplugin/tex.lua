vim.b[0].localCommands = {
    { source = "tex", name = "Build project", command = "silent VimtexCompileSS" },
    { source = "tex", name = "Table of content", command = "VimtexTocToggle" },
    { source = "tex", name = "Forward Search", command = "TexlabForward" },
    { source = "tex", name = "Word Count", command = "VimtexCountWord" },
}

Map("n", "<localleader><localleader>", "<cmd>TexlabForward<cr>", { buffer = 0, silent = true })

Map("n", "KK", "<cmd>VimtexDocPackage<cr>", { buffer = 0 })

Map({ "x", "o" }, "as", "<plug>(vimtex-aP)", { buffer = 0, remap = true })
Map({ "x", "o" }, "is", "<plug>(vimtex-iP)", { buffer = 0, remap = true })
Map({ "n", "x", "o" }, "[s", "<cmd>let g:dirJumps='s'<cr>m`<plug>(vimtex-[[)zz", { buffer = 0 })
Map({ "x", "o" }, "als", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-[])', '(vimtex-aP)')<cr>", { buffer = 0 })
Map({ "x", "o" }, "ils", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-[])', '(vimtex-iP)')<cr>", { buffer = 0 })
Map({ "n", "x", "o" }, "]s", "<cmd>let g:dirJumps='s'<cr>m`<plug>(vimtex-]])zz", { buffer = 0 })
Map({ "x", "o" }, "ins", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]])', '(vimtex-iP)')<cr>", { buffer = 0 })
Map({ "x", "o" }, "ans", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]])', '(vimtex-aP)')<cr>", { buffer = 0 })

Map({ "x", "o" }, "aj", "<plug>(vimtex-ae)", { buffer = 0, remap = true })
Map({ "x", "o" }, "ij", "<plug>(vimtex-ie)", { buffer = 0, remap = true })
Map({ "n", "x", "o" }, "[j", "<cmd>let g:dirJumps='m'<cr>m`<plug>(vimtex-[m)zz", { buffer = 0 })
Map({ "x", "o" }, "alj", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-[M)', '(vimtex-ae)')<cr>", { buffer = 0 })
Map({ "x", "o" }, "ilj", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-[M)', '(vimtex-ie)')<cr>", { buffer = 0 })
Map({ "n", "x", "o" }, "]j", "<cmd>let g:dirJumps='m'<cr>m`<plug>(vimtex-]m)zz", { buffer = 0 })
Map({ "x", "o" }, "anj", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]m)', '(vimtex-ae)')<cr>", { buffer = 0 })
Map({ "x", "o" }, "inj", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]m)', '(vimtex-ie)')<cr>", { buffer = 0 })

Map({ "x", "o" }, "am", "<plug>(vimtex-a$)", { buffer = 0, remap = true })
Map({ "x", "o" }, "im", "<plug>(vimtex-i$)", { buffer = 0, remap = true })
Map({ "n", "x", "o" }, "[m", "<cmd>let g:dirJumps='n'<cr>m`<plug>(vimtex-[n)zz", { buffer = 0 })
Map({ "x", "o" }, "alm", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-[l)', '(vimtex-a$)')<cr>", { buffer = 0 })
Map({ "x", "o" }, "ilm", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-[l)', '(vimtex-i$)')<cr>", { buffer = 0 })
Map({ "n", "x", "o" }, "]m", "<cmd>let g:dirJumps='n'<cr>m`<plug>(vimtex-]n)zz", { buffer = 0 })
Map({ "x", "o" }, "anm", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]n)', '(vimtex-a$)')<cr>", { buffer = 0 })
Map({ "x", "o" }, "inm", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]n)', '(vimtex-i$)')<cr>", { buffer = 0 })

Map("n", "dpc", "<plug>(vimtex-env-delete)", { buffer = 0, remap = true })
Map("n", "dpe", "<plug>(vimtex-cmd-delete)", { buffer = 0, remap = true })
Map("n", "dp$", "<plug>(vimtex-cmd-delete-math)", { buffer = 0, remap = true })
Map("n", "dpd", "<plug>(vimtex-delim-delete)", { buffer = 0, remap = true })
Map("n", "cpc", "<plug>(vimtex-env-change)", { buffer = 0, remap = true })
Map("n", "cpe", "<plug>(vimtex-cmd-change)", { buffer = 0, remap = true })
Map("n", "cp$", "<plug>(vimtex-cmd-change-math)", { buffer = 0, remap = true })
Map("n", "cpd", "<plug>(vimtex-delim-change)", { buffer = 0, remap = true })

vim.api.nvim_buf_set_option(0, "textwidth", 100)

-- vim.cmd([[let g:vimtex_toc_config=[{"mode":3, "split_width":60}]]])
vim.g.AngryReviewerEnglish = "british"
vim.g.tex_flavor = "latex"
vim.g.vimtex_quickfix_mode = 0
vim.g.vimtex_view_general_viewer = "zathura"
vim.g.vimtex_view_forward_search_on_start = 1
vim.g.vimtex_view_automatic = 0
vim.g.vimtex_compiler_latexmk = {
    ["callback"] = 1,
    ["continuous"] = 0,
    ["executable"] = "latexmk",
    ["hooks"] = {},
    ["options"] = { "-pdf", "-verbose", "-file-line-error", "-synctex=1", "-interaction=nonstopmode" },
}

vim.opt_local.conceallevel = 2
vim.g.vimtex_syntax_conceal = {
    ["accents"] = true,
    ["cites"] = true,
    ["fancy"] = true,
    ["greek"] = true,
    ["math_bounds"] = true,
    ["math_delimiters"] = true,
    ["math_fracs"] = true,
    ["math_super_sub"] = false,
    ["math_symbols"] = true,
    ["sections"] = true,
    ["styles"] = true,
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

vim.wo.winbar = "%{%v:lua.GPS_Bar()%}"
