return function()
    return {
        {
            source = "xmonad",
            name = "Reload XMonad",
            func = function()
                XmoReload = Terminal:new({
                    jobname = "Reload Xmonad",
                    dir = "/home/oleete/.config/xmonad",
                    cmd = "stack install && xmonad --recompile && xmonad --restart",
                    id = 15,
                })
                XmoReload:set_background()
            end
        },
    }
end
