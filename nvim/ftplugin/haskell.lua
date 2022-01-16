-- vim.cmd("TSDisableAll highlight haskell")
nnoremap("<leader>mm", [[<cmd>silent !kittyOneShot maketerm "cd /home/oleete/.config/xmonad; stack install; stack install; xmonad --recompile; xmonad --restart"<cr>]], "Compile and flash")
