-- Leader Mapping
vim.opt.timeoutlen = 500
vim.api.nvim_set_keymap("", "<BackSPACE>", "<Nop>", { noremap = true })
vim.api.nvim_set_keymap("", "<SPACE>", "<Nop>", { noremap = true })
vim.api.nvim_set_keymap("", "<BackSPACE>", "<Nop>", { noremap = true })
vim.api.nvim_set_var("mapleader", " ")
vim.api.nvim_set_var("maplocalleader", "\\")

-- Un-Mappings
vim.api.nvim_set_keymap("", "v", "<nop>", { nowait = true })
vim.api.nvim_set_keymap("", "V", "<nop>", { nowait = true })
vim.api.nvim_set_keymap("", "<c-v>", "<nop>", { nowait = true })
vim.api.nvim_set_keymap("", "dd", "<nop>", { nowait = true })
vim.api.nvim_set_keymap("", "cc", "<nop>", { nowait = true })
vim.api.nvim_set_keymap("", "yy", "<nop>", { nowait = true })
vim.api.nvim_set_keymap("", "z", "<nop>", { nowait = true })

vim.api.nvim_set_keymap("", "Y", "<nop>", {})
vim.api.nvim_set_keymap("", "C", "<nop>", {})
vim.api.nvim_set_keymap("", "D", "<nop>", {})
vim.api.nvim_set_keymap("", "S", "<nop>", {})


-- NOTE: _, =, |, ^, ¬ and # are free to map

-- Mappings
vim.api.nvim_set_keymap("n", ";", ":", { noremap = true })
vim.api.nvim_set_keymap("n", ":", ";", { noremap = true })
vim.api.nvim_set_keymap("x", ";", ":", { noremap = true })
vim.api.nvim_set_keymap("x", ":", ";", { noremap = true })
vim.api.nvim_set_keymap("o", ";", ":", { noremap = true })
vim.api.nvim_set_keymap("o", ":", ";", { noremap = true })

vim.api.nvim_set_keymap("n", "<m-f>", ";", { noremap = true })
vim.api.nvim_set_keymap("n", "<m-F>", ",", { noremap = true })
vim.api.nvim_set_keymap("n", "<m-t>", ";", { noremap = true })
vim.api.nvim_set_keymap("n", "<m-T>", ",", { noremap = true })
vim.api.nvim_set_keymap("x", "<m-f>", ";", { noremap = true })
vim.api.nvim_set_keymap("x", "<m-F>", ",", { noremap = true })
vim.api.nvim_set_keymap("x", "<m-t>", ";", { noremap = true })
vim.api.nvim_set_keymap("x", "<m-T>", ",", { noremap = true })

vim.api.nvim_set_keymap("n", "g<c-a>", "v<c-a>", { noremap = true })
vim.api.nvim_set_keymap("n", "g<c-x>", "v<c-x>", { noremap = true })
vim.api.nvim_set_keymap("n", "+", "<c-a>", { noremap = true })
vim.api.nvim_set_keymap("n", "-", "<c-x>", { noremap = true })
vim.api.nvim_set_keymap("n", "g+", "v<c-a>", { noremap = true })
vim.api.nvim_set_keymap("n", "g-", "v<c-x>", { noremap = true })
vim.api.nvim_set_keymap("x", "+", "<c-a>", { noremap = true })
vim.api.nvim_set_keymap("x", "-", "<c-x>", { noremap = true })
vim.api.nvim_set_keymap("x", "g+", "g<c-a>", { noremap = true })
vim.api.nvim_set_keymap("x", "g-", "g<c-x>", { noremap = true })

vim.api.nvim_set_keymap("x", "y", "m1y`1", { noremap = true, nowait = true})
vim.api.nvim_set_keymap("x", "d", "d", { noremap = true, nowait = true})
vim.api.nvim_set_keymap("x", "c", "c", { noremap = true, nowait = true})

vim.api.nvim_set_keymap("n", "x", "V", { noremap = true })
vim.api.nvim_set_keymap("n", "X", "V", { noremap = true })
vim.api.nvim_set_keymap("n", "C", "<c-v>j", { noremap = true })
vim.api.nvim_set_keymap("n", "<m-C>", "<c-v>k", { noremap = true })
vim.api.nvim_set_keymap("n", "<M-v>", "v", { noremap = true })

vim.api.nvim_set_keymap("x", "x", "j$", { noremap = true })
vim.api.nvim_set_keymap("x", "X", "<esc>`<kV`>", { noremap = true })
vim.api.nvim_set_keymap("x", "C", "j", { noremap = true })
vim.api.nvim_set_keymap("x", "<m-C>", "<esc>`<k<c-v>`>", { noremap = true })
vim.api.nvim_set_keymap("x", "<M-v>", "v", { noremap = true })
vim.api.nvim_set_keymap("x", "<M-;>", "o", { noremap = true })

vim.api.nvim_set_keymap("n", "<m-c>", [["_c]], { noremap = true })
vim.api.nvim_set_keymap("n", "<m-d>", [["_d]], { noremap = true })
vim.api.nvim_set_keymap("x", "<m-c>", [["_c]], { noremap = true })
vim.api.nvim_set_keymap("x", "<m-d>", [["_d]], { noremap = true })

vim.api.nvim_set_keymap("n", "<m-c><m-c>", [["_cc]], { noremap = true })
vim.api.nvim_set_keymap("n", "<m-d><m-d>", [["_dd]], { noremap = true })
vim.api.nvim_set_keymap("x", "<m-c><m-c>", [["_cc]], { noremap = true })
vim.api.nvim_set_keymap("x", "<m-d><m-d>", [["_dd]], { noremap = true })

vim.api.nvim_set_keymap("n", "<m-o>", "m1o<esc>`1", { noremap = true })
vim.api.nvim_set_keymap("n", "<m-O>", "m1O<esc>`1", { noremap = true })

vim.api.nvim_set_keymap("n", "]", "<cmd>WhichKey ] n<cr>", { noremap = true })
vim.api.nvim_set_keymap("x", "]", "<cmd>WhichKey ] x<cr>", { noremap = true })
vim.api.nvim_set_keymap("n", "[", "<cmd>WhichKey [ n<cr>", { noremap = true })
vim.api.nvim_set_keymap("x", "[", "<cmd>WhichKey [ x<cr>", { noremap = true })
vim.api.nvim_set_keymap("n", ",", "<cmd>WhichKey g, n<cr>", { noremap = true })
vim.api.nvim_set_keymap("x", ",", "<cmd>WhichKey g, x<cr>", { noremap = true })
vim.api.nvim_set_keymap("x", "I", "I", { noremap = true })
vim.api.nvim_set_keymap("x", "A", "A", { noremap = true })

vim.api.nvim_set_keymap("n", "mm", "<cmd>lua require'harpoon.mark'.add_file()<cr>", { noremap = true })
vim.api.nvim_set_keymap("n", "ma", "<cmd>lua require'harpoon.ui'.nav_file(1)<cr>", { noremap = true })
vim.api.nvim_set_keymap("n", "mr", "<cmd>lua require'harpoon.ui'.nav_file(2)<cr>", { noremap = true })
vim.api.nvim_set_keymap("n", "ms", "<cmd>lua require'harpoon.ui'.nav_file(3)<cr>", { noremap = true })
vim.api.nvim_set_keymap("n", "mt", "<cmd>lua require'harpoon.ui'.nav_file(4)<cr>", { noremap = true })
vim.api.nvim_set_keymap("n", "M", "<cmd>lua require'harpoon.ui'.toggle_quick_menu()<cr>", { noremap = true })

vim.api.nvim_set_keymap("n", "<c-_>", "g,cc", {})
vim.api.nvim_set_keymap("x", "<c-_>", "g,c", {})

vim.api.nvim_set_keymap("n", "£", [[:exe "let @/='" . expand("<cWORD>") . "']"<cr>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap("n", "<c-p>", "a<c-p>", {})

vim.cmd([[inoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[inoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[snoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[snoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])

vim.api.nvim_set_keymap("i", "<c-g>", "<c-o>%", { noremap = true })
vim.api.nvim_set_keymap("i", "<c-s>", "<cmd>lua require('lsp_signature').toggle_float_win()<CR>", { noremap = true })

vim.api.nvim_set_keymap("i", ",", ",<c-g>u", { noremap = true })
vim.api.nvim_set_keymap("i", ".", ".<c-g>u", { noremap = true })
vim.api.nvim_set_keymap("i", "!", "!<c-g>u", { noremap = true })
vim.api.nvim_set_keymap("i", "?", "?<c-g>u", { noremap = true })


-- Panel Specific Mappings
vim.cmd([[augroup panelMappings
    autocmd!
    autocmd filetype qf                  map <buffer> <esc> <cmd>q<cr>
    autocmd filetype help                map <buffer> <esc> <cmd>q<cr>
    autocmd filetype vim-plug            map <buffer> <esc> <cmd>q<cr>
    autocmd filetype juliadoc            map <buffer> <esc> <cmd>q<cr>
    autocmd filetype undotree            map <buffer> <esc> <cmd>UndotreeHide<cr>
    autocmd filetype lspinfo             map <buffer> <esc> <cmd>q<cr>
    autocmd filetype DiffviewFiles       map <buffer> <esc> <cmd>DiffviewClose<cr>
    autocmd filetype DiffviewFileHistory map <buffer> <esc> <cmd>DiffviewClose<cr>
    autocmd filetype tsplayground        map <buffer> <esc> <cmd>q<cr>
    autocmd filetype harpoon-menu        map <buffer> <esc> <cmd>wq<cr>
augroup END]])

vim.api.nvim_set_var("wordmotion_prefix", "$")

-- UnMap Plugins
vim.api.nvim_set_var("kitty_navigator_no_mappings", true)
vim.api.nvim_set_var("UnconditionalPaste_no_mappings", true)
vim.api.nvim_set_var("caser_no_mappings	=", true)
vim.api.nvim_set_var("textobj_markdown_no_default_key_mappings", true)
vim.api.nvim_set_var("julia_blocks", false)
