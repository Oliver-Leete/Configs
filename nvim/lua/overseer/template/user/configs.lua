local overseer = require("overseer")

return {
    condition = {
        dir = "/home/oleete/.config",
    },
    generator = function(_, cb)
        cb({
            {
                name = "Build and Reload XMonad",
                builder = function()
                    return {
                        name = "Reload XMonad",
                        cwd = "/home/oleete/.config/xmonad",
                        cmd = "/home/oleete/.config/bin/xmonadRebuild",
                        components = { "default", "unique" }
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
                        components = { "default", "unique" },
                    }
                end,
                priority = 60,
                params = {},
            },
            {
                name = "Log XMonad Stack",
                builder = function()
                    return {
                        name = "Log XMonad Stack",
                        cmd = { "/home/oleete/.cabal/bin/xmonadctl-exe", "dump-stack" },
                        components = { "default", "unique" },
                    }
                end,
                priority = 61,
                params = {},
            },
            {
                name = "Log XMonad Stack Full",
                builder = function()
                    return {
                        name = "Log XMonad Full Stack",
                        cmd = { "/home/oleete/.cabal/bin/xmonadctl-exe", "dump-full-stack" },
                        components = { "default", "unique" }
                    }
                end,
                priority = 61,
                params = {},
            },
            {
                name = "View xsession Logs",
                builder = function()
                    return {
                        name = "View xsession Logs",
                        cmd = "tail --follow --retry ~/.xsession-errors | less -S",
                        components = { "default", "unique",
                            {
                                "user.start_open",
                                goto_prev = true,
                            },
                        }
                    }
                end,
                priority = 60,
                params = {},
            }
        })
    end
}
