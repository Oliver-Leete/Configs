-- vim.cmd("TSDisableAll highlight haskell")
vim.b[0].localCommands = {
	{
		source = "haskell",
		name = "Reload XMonad",
		command = [[<cmd>silent !kittyOneShot maketerm "cd /home/oleete/.config/xmonad; stack install; stack install; xmonad --recompile; xmonad --restart"<cr>]],
	},
}
