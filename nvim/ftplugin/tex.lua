vim.api.nvim_command([[let g:tex_flavor='latex'
let g:vimtex_view_method='zathura'
let g:vimtex_quickfix_mode=0
let g:vimtex_view_automatic=1
let g:vimtex_view_forward_search_on_start=1

autocmd FileType markdown let b:compe_latex_insert_code = v:true
autocmd FileType tex let b:compe_latex_insert_code = v:true
let g:vimtex_compiler_latexmk = {
    \ 'build_dir' : '',
    \ 'callback' : 1,
    \ 'continuous' : 0,
    \ 'executable' : 'latexmk',
    \ 'hooks' : [],
    \ 'options' : [
    \   '-verbose',
    \   '-file-line-error',
    \   '-synctex=1',
    \   '-interaction=nonstopmode',
    \ ],
    \}
]])

require("which-key").register({
    ["<localleader>"] = {
        r = {"<cmd>Telescope bibtex bibtex<cr>", "Headings"}
    }
}, {buffer=0})
