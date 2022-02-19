-- vim.cmd("TSDisableAll highlight haskell")
mapxName.group(mapxName.buffer, function()
nnoremap("<leader>mm", [[<cmd>silent !kittyOneShot maketerm "cd /home/oleete/.config/xmonad; stack install; stack install; xmonad --recompile; xmonad --restart"<cr>]], "Compile and flash")
end)
