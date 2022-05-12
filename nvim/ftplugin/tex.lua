mapxName.group(mapxName.buffer, function()
nnoremap("<localleader><localleader>", "<cmd>TexlabForward<cr>", "Forward Search", mapxName.silent)
nnoremap("<localleader>r", "<cmd>Telescope bibtex bibtex theme=get_cursor<cr>", "References")
nnoremap("<localleader>c", "<cmd>VimtexTocToggle<cr>", "Open TOC")
nnoremap("<localleader>l", "<cmd>call g:custom_toc1.toggle()<cr>", "Open TOL")
nnoremap("<localleader>t", "<cmd>call g:custom_toc2.toggle()<cr>", "Open TODO List")
nnoremap("<localleader>w", "<cmd>VimtexCountWord<cr>", "Word Count")
nnoremap("<localleader>W", "<cmd>VimtexCountWord!<cr>", "Word Count Report")
nnoremap("<localleader>m", "<cmd>VimtexToggleMain<cr>", "Toggle the Main File")
nnoremap("<localleader>g", "<cmd>TexlabForward<cr>", "Forward Search")
nnoremap("<localleader>i", "A% chktex ", "Chktex Ignore")
nnoremap("<localleader>s", "<cmd>lua require'telescope.builtin'.symbols{sources={'latex'}}<cr>", "Insert Symbols")
nnoremap("<localleader>a", "<cmd>AngryReviewer<cr><cmd>Trouble quickfix<cr>", "Bring Forth The Angry One")
nnoremap("<localleader>,", function() require'telescope'.extensions.dict.synonyms() end, "Synonyms")

nnoremap("K", "<cmd>VimtexDocPackage<cr>")
nnoremap("<leader>M", [[<cmd>silent !kittyOneShot buildterm "latexmk -verbose -file-line-error -synctex=1 -interaction=nonstopmode"<cr>]], "Build in Terminal")
nnoremap("<leader>mm", "<cmd>VimtexCompileSS<cr>", "Build Project")
nnoremap("<leader>mM", "<cmd>ProjectDo TexlabBuild<cr>", "Build Lowest Subfile")
nnoremap("<leader>mo", "<cmd>VimtexCompile<cr>", "Toggle Continuous Building")
nnoremap("<leader>mc", "<cmd>VimtexClean<cr>", "Clear Build Files")
xnoremap("<leader>mm", "<cmd>VimtexCompileSelected<cr>", "Build Selected")

nmap("[s", "<cmd>let g:dirJumps='s'<cr>m`<plug>(vimtex-[[)zz", "Tex Section Start")
nmap("[S", "<cmd>let g:dirJumps='S'<cr>m`<plug>(vimtex-[])zz", "Tex Section End")
nmap("[m", "<cmd>let g:dirJumps='m'<cr>m`<plug>(vimtex-[m)zz", "EnviroMent Start")
nmap("[M", "<cmd>let g:dirJumps='M'<cr>m`<plug>(vimtex-[M)zz", "EnviroMent End")
nmap("[n", "<cmd>let g:dirJumps='n'<cr>m`<plug>(vimtex-[n)zz", "Number Start")
nmap("[N", "<cmd>let g:dirJumps='N'<cr>m`<plug>(vimtex-[N)zz", "Number End")
nmap("[r", "<cmd>let g:dirJumps='r'<cr>m`<plug>(vimtex-[r)zz", "Frame Start")
nmap("[R", "<cmd>let g:dirJumps='R'<cr>m`<plug>(vimtex-[R)zz", "Frame End")
nmap("[]", "<cmd>let g:dirJumps='S'<cr>m`<plug>(vimtex-[[)zz", "Tex Section Start")
nmap("[[", "<cmd>let g:dirJumps='s'<cr>m`<plug>(vimtex-[])zz", "Tex Section End")

nmap("]s", "<cmd>let g:dirJumps='s'<cr>m`<plug>(vimtex-]])zz", "Tex Section Start")
nmap("]S", "<cmd>let g:dirJumps='S'<cr>m`<plug>(vimtex-][)zz", "Tex Section End")
nmap("]m", "<cmd>let g:dirJumps='m'<cr>m`<plug>(vimtex-]m)zz", "EnviroMent Start")
nmap("]M", "<cmd>let g:dirJumps='M'<cr>m`<plug>(vimtex-]M)zz", "EnviroMent End")
nmap("]n", "<cmd>let g:dirJumps='n'<cr>m`<plug>(vimtex-]n)zz", "Number Start")
nmap("]N", "<cmd>let g:dirJumps='N'<cr>m`<plug>(vimtex-]N)zz", "Number End")
nmap("]r", "<cmd>let g:dirJumps='r'<cr>m`<plug>(vimtex-]r)zz", "Frame Start")
nmap("]R", "<cmd>let g:dirJumps='R'<cr>m`<plug>(vimtex-]R)zz", "Frame End")
nmap("][", "<cmd>let g:dirJumps='s'<cr>m`<plug>(vimtex-][)zz", "Tex Section End")
nmap("]]", "<cmd>let g:dirJumps='S'<cr>m`<plug>(vimtex-]])zz", "Tex Section Start")

xmap("iP", "<plug>(vimtex-iP)", "Section")
xmap("im", "<plug>(vimtex-ie)", "Section")
xmap("in", "<plug>(vimtex-i$)", "Section")
xmap("aP", "<plug>(vimtex-aP)", "Section")
xmap("am", "<plug>(vimtex-ae)", "Section")
xmap("an", "<plug>(vimtex-a$)", "Section")
xnoremap("i]P", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]])', '(vimtex-iP)')<cr>", "Section")
xnoremap("i]m", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]m)', '(vimtex-ie)')<cr>", "Environment")
xnoremap("i]n", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]n)', '(vimtex-i$)')<cr>", "Maths")
xnoremap("i[P", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[])', '(vimtex-[])', '(vimtex-iP)')<cr>", "Section")
xnoremap("i[m", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[m)', '(vimtex-[M)', '(vimtex-ie)')<cr>", "Environment")
xnoremap("i[n", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[n)', '(vimtex-[N)', '(vimtex-i$)')<cr>", "Maths")
xnoremap("a]P", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]])', '(vimtex-aP)')<cr>", "Section")
xnoremap("a]m", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]m)', '(vimtex-ae)')<cr>", "Environment")
xnoremap("a]n", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]n)', '(vimtex-a$)')<cr>", "Maths")
xnoremap("a[P", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[])', '(vimtex-[])', '(vimtex-aP)')<cr>", "Section")
xnoremap("a[m", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[m)', '(vimtex-[M)', '(vimtex-ae)')<cr>", "Environment")
xnoremap("a[n", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[n)', '(vimtex-[N)', '(vimtex-a$)')<cr>", "Maths")

omap("iP", "<plug>(vimtex-iP)", "Section")
omap("im", "<plug>(vimtex-ie)", "Section")
omap("in", "<plug>(vimtex-i$)", "Section")
omap("aP", "<plug>(vimtex-aP)", "Section")
omap("am", "<plug>(vimtex-ae)", "Section")
omap("an", "<plug>(vimtex-a$)", "Section")
onoremap("i]P", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]])', '(vimtex-iP)')<cr>", "Section")
onoremap("i]m", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]m)', '(vimtex-ie)')<cr>", "Environment")
onoremap("i]n", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]n)', '(vimtex-i$)')<cr>", "Maths")
onoremap("i[P", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[])', '(vimtex-[])', '(vimtex-iP)')<cr>", "Section")
onoremap("i[m", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[m)', '(vimtex-[M)', '(vimtex-ie)')<cr>", "Environment")
onoremap("i[n", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[n)', '(vimtex-[N)', '(vimtex-i$)')<cr>", "Maths")
onoremap("a]P", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]])', '(vimtex-aP)')<cr>", "Section")
onoremap("a]m", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]m)', '(vimtex-ae)')<cr>", "Environment")
onoremap("a]n", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]n)', '(vimtex-a$)')<cr>", "Maths")
onoremap("a[P", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[])', '(vimtex-[])', '(vimtex-aP)')<cr>", "Section")
onoremap("a[m", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[m)', '(vimtex-[M)', '(vimtex-ae)')<cr>", "Environment")
onoremap("a[n", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[n)', '(vimtex-[N)', '(vimtex-a$)')<cr>", "Maths")
end)

if vim.api.nvim_get_var("panelRepeat") == "q" then
    vim.api.nvim_set_var("panelRepeat", "c")
end

vim.api.nvim_buf_set_option(0, "textwidth", 100)

-- vim.cmd([[let g:vimtex_toc_config=[{"mode":3, "split_width":60}]]])
vim.cmd([[let g:vimtex_quickfix_mode=0]])
vim.cmd([[let g:vimtex_view_general_viewer='zathura']])
vim.cmd([[let g:vimtex_view_forward_search_on_start=1]])
vim.cmd([[let g:vimtex_view_automatic = 0]])
vim.g.vimtex_compiler_latexmk = {
    ["callback"] = 1,
    ["continuous"] = 0,
    ["executable"] = 'latexmk',
    ["hooks"] = {},
    ["options"] = { '-verbose', '-file-line-error', '-synctex=1', '-interaction=nonstopmode'}
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
vim.cmd([[
    let g:custom_toc1 = vimtex#toc#new({ 'name':'Tabel of labels', 'layers' : ['label'], 'todo_sorted' : 0, 'show_help' : 0, 'show_numbers' : 0, 'mode' : 1})
    let g:custom_toc2 = vimtex#toc#new({ 'name':'TODO', 'layers' : ['todo'], 'todo_sorted' : 0, 'show_help' : 0, 'show_numbers' : 0, 'mode' : 1})
]])

vim.g.AngryReviewerEnglish = 'british'

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
