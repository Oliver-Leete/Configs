nnoremap("<localleader><localleader>", "<cmd>TexlabForward<cr>", "Forward Search", mapxName.silent) 
nnoremap("<localleader>r", "<cmd>Telescope bibtex bibtex theme=get_cursor<cr>", "References")
nnoremap("<localleader>c", "<cmd>VimtexTocToggle<cr>", "Open TOC")
nnoremap("<localleader>v", "<cmd>VimtexView<cr>", "View Document")
nnoremap("<localleader>w", "<cmd>VimtexCountWord<cr>", "Word Count")
nnoremap("<localleader>W", "<cmd>VimtexCountWord!<cr>", "Word Count Report")
nnoremap("<localleader>m", "<cmd>VimtexToggleMain<cr>", "Toggle the Main File")
nnoremap("<localleader>g", "<cmd>TexlabForward<cr>", "Forward Search")
nnoremap("<localleader>i", "A% chktex ", "Chktex Ignore")
nnoremap("<localleader>s", "<cmd>lua require'telescope.builtin'.symbols{sources={'latex'}}<cr>", "Insert Symbols")
nnoremap("<localleader>a", "<cmd>AngryReviewer<cr>", "Bring Forth The Angry One")

nnoremap("<leader>M", [[<cmd>silent !kittyOneShot buildterm "latexmk -verbose -file-line-error -synctex=1 -interaction=nonstopmode"<cr>]], "Build in Terminal")
nnoremap("<leader>mm", "<cmd>VimtexCompileSS<cr>", "Build Project")
nnoremap("<leader>mM", "<cmd>ProjectDo TexlabBuild<cr>", "Build Lowest Subfile")
nnoremap("<leader>mo", "<cmd>VimtexCompile<cr>", "Toggle Continuous Building")
nnoremap("<leader>mc", "<cmd>VimtexClean<cr>", "Clear Build Files")
nnoremap("<leader>mg", "<cmd>compiler textidote<cr><cmd>lmake!<cr><cmd>Trouble loclist<cr>", "Grammer Checking")
xnoremap("<leader>mm", "<cmd>VimtexCompileSelected<cr>", "Build Selected")

nnoremap("<leader>c", "<cmd>let panelRepeat='c'<cr><cmd>VimtexTocToggle<cr>", "Open TOC" )

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

xnoremap("inP", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]])', '(vimtex-iP)')<cr>", "Section")
xnoremap("inm", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]m)', '(vimtex-ie)')<cr>", "Environment")
xnoremap("inn", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]n)', '(vimtex-i$)')<cr>", "Maths")
xnoremap("iNP", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[])', '(vimtex-[])', '(vimtex-iP)')<cr>", "Section")
xnoremap("iNm", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[m)', '(vimtex-[M)', '(vimtex-ie)')<cr>", "Environment")
xnoremap("iNn", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[n)', '(vimtex-[N)', '(vimtex-i$)')<cr>", "Maths")
xnoremap("anP", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]])', '(vimtex-aP)')<cr>", "Section")
xnoremap("anm", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]m)', '(vimtex-ae)')<cr>", "Environment")
xnoremap("ann", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]n)', '(vimtex-a$)')<cr>", "Maths")
xnoremap("aNP", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[])', '(vimtex-[])', '(vimtex-aP)')<cr>", "Section")
xnoremap("aNm", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[m)', '(vimtex-[M)', '(vimtex-ae)')<cr>", "Environment")
xnoremap("aNn", ":<c-u>call v:lua.plug_targets_back(v:count, '(vimtex-[n)', '(vimtex-[N)', '(vimtex-a$)')<cr>", "Maths")

if vim.api.nvim_get_var("panelRepeat") == "q" then
    vim.api.nvim_set_var("panelRepeat", "c")
end

vim.api.nvim_buf_set_option(0, "textwidth", 100)

-- vim.cmd([[let g:vimtex_toc_config=[{"mode":3, "split_width":60}]]])
vim.cmd([[let g:vimtex_quickfix_mode=0]])
vim.cmd([[let g:vimtex_view_method='zathura']])
vim.cmd([[let g:vimtex_view_forward_search_on_start=1]])
vim.cmd([[let g:vimtex_view_automatic = 0]])
vim.cmd(
    [[let g:vimtex_compiler_latexmk = { 'build_dir' : '', 'callback' : 1, 'continuous' : 0, 'executable' : 'latexmk', 'hooks' : [], 'options' : [ '-verbose', '-file-line-error', '-synctex=1', '-interaction=nonstopmode', ] }]]
)
vim.cmd([[let g:vimtex_grammar_textidote={'jar':'~/.local/bin/textidote.jar', 'args':''}]])
vim.cmd([[set errorformat=]])
vim.cmd([[set errorformat+=%f(L%lC%c-L%\\d%\\+C%\\d%\\+):\ %m]])
vim.cmd([[set errorformat+=%-G%.%#]])

vim.g.AngryReviewerEnglish = {'british'}

vim.g.projectionist_heuristics = {
    ["OML-Thesis.tex"] = {
        ["*.tex"] = {
            type = "chapMain",
            alternate = "OML-Thesis.tex",
        },
        ["Scripts/*.tex"] = {
            type = "scripts",
            alternate = "OML-Thesis.tex",
        },
        ["1/*.tex"] = {
            type = "chapOne",
            alternate = "IntroductiontoAM.tex",
        },
        ["2/*.tex"] = {
            type = "chapTwo",
            alternate = "PowderBedFusion.tex",
        },
        ["3/*.tex"] = {
            type = "chapThree",
            alternate = "ModellingofAM.tex",
        },
        ["4/*.tex"] = {
            type = "chapFour",
            alternate = "PowderCharacterisation.tex",
        },
        ["5/*.tex"] = {
            type = "chapFive",
            alternate = "PowderModel.tex",
        },
        ["6/*.tex"] = {
            type = "chapSix",
            alternate = "MachineModel.tex",
        },
        ["7/*.tex"] = {
            type = "chapSeven",
            alternate = "ProcessModel.tex",
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

if not PdfOpened then
    PdfOpened = true
    vim.cmd("VimtexView")
end
