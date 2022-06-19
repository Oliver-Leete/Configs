return function()
    return {
        {
            source = "xmonad",
            name = "Reload XMonad",
            func = function()
                Harp_Term_2:send_open("cd /home/oleete/.config/xmonad; stack install; stack install; xmonad --recompile; xmonad --restart; cd -", true, 2)
            end
        },
    }
end
