return {
    {
        source = "haskell",
        name = "Reload XMonad",
        command = [[silent !kittyOneShot "cd /home/oleete/.config/xmonad; stack install; stack install; xmonad --recompile; xmonad --restart"]],
    },
}
