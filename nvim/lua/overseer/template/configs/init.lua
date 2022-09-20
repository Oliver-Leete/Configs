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
                        components = {"default", "unique"}
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
                        components = {"default", "unique"}
                    }
                end,
                priority = 60,
                params = {},
            },
            {
                name = "View xsession Logs",
                builder = function()
                    return {
                        name = "View xsession Logs",
                        cmd = "tail --follow --retry ~/.xsession-errors | less -S",
                        components = {"default", "unique"}
                    }
                end,
                priority = 60,
                params = {},
            }
        }
    end
}
