require("which-key").register({
    ["<localleader>"] = {
        r = {"<cmd>Telescope bibtex bibtex<cr>", "References"},
        x = {"<cmd>VimtexTocToggle<cr>", "Open TOC"},
        v = {"<cmd>VimtexView<cr>", "View Document"},
        c = {"<cmd>VimtexCountWord<cr>", "Word Count"},
        C = {"<cmd>VimtexCountWord!<cr>", "Word Count Report"},
        m = {"<cmd>VimtexToggleMain<cr>", "Toggle the Main File"},
    },
    ["<leader>"] = {
        M = {[[<cmd>2TermExec cmd="latexmk -verbose -file-line-error -synctex=1 -interaction=nonstopmode"<cr>]], "Build in Terminal"},
        m = {
            m = {"<cmd>VimtexCompileSS<cr>", "Build Once"},
            o = {"<cmd>VimtexCompile<cr>", "Toggle Continuous Building"},
            c = {"<cmd>VimtexClean<cr>", "Clear Build Files"},
        },
    },
}, {buffer=0})

require("which-key").register({
    ["<localleader>"] = {
    },
    ["<leader>"] = {
        m = {
            m = {"<cmd>VimtexCompileSelected<cr>", "Build Selected"},
        },
    },
}, {buffer=0, mode = "v"})

vim.cmd([[let g:vimtex_quickfix_mode=0]])
vim.cmd([[let g:vimtex_view_method='zathura']])
vim.cmd([[let g:vimtex_view_automatic=1]])
vim.cmd([[let g:vimtex_view_forward_search_on_start=1]])
vim.cmd([[let b:compe_latex_insert_code = v:true]])
vim.cmd([[let g:vimtex_compiler_latexmk = { 'build_dir' : '', 'callback' : 1, 'continuous' : 0, 'executable' : 'latexmk', 'hooks' : [], 'options' : [ '-verbose', '-file-line-error', '-synctex=1', '-interaction=nonstopmode', ] }]])
