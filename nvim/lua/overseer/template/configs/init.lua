local overseer = require("overseer")

return {
    condition = {
        dir = "/home/oleete/.config",
    },
    generator = function(_)
        return {
            {
                name = "Reload XMonad",
                builder = function()
                    return {
                        name = "Reload XMonad",
                        cwd = "/home/oleete/.config/xmonad",
                        cmd = "/home/oleete/.config/bin/xmonadRebuild",
                    }
                end,
                priority = 60,
                tags = { overseer.TAG.BUILD },
                params = {},
            },
            {
                name = "Reload Kitty",
                builder = function()
                    return {
                        name = "Reload Kitty",
                        cmd = "pkill -10 kitty",
                    }
                end,
                priority = 60,
                params = {},
            }
        }
    end
}
