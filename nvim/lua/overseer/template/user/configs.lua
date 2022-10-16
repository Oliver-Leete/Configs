local overseer = require("overseer")

return {
    condition = {
        dir = "/home/oleete/.config",
    },
    generator = function(_, cb)
        cb({
            {
                name = "Source File",
                builder = function()
                    vim.cmd.source(vim.fn.expand("%"))
                    return { cmd = "", name = "", components = { "user.dispose_now" }, }
                end,
                priority = 59,
                params = {},
                conditions = { filetype = "lua" }
            },
            {
                name = "Source NVim Init",
                builder = function()
                    vim.cmd.source("/home/oleete/.config/nvim/init.lua")
                    return { cmd = "", name = "", components = { "user.dispose_now" }, }
                end,
                priority = 59,
                params = {},
            },
            {
                name = "Source NVim Module",
                builder = function()
                    require("telescope.builtin").reloader(require("telescope.themes").get_ivy())
                    return { cmd = "", name = "", components = { "user.dispose_now" }, }
                end,
                priority = 59,
                params = {},
            },
            {
                name = "Reload XMonad",
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
                        components = { "default", "unique" }
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
                        components = { "default_hide", "unique" }
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
                        components = { "default_hide", "unique" }
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
                        components = { "default", "unique" }
                    }
                end,
                priority = 60,
                params = {},
            }
        })
    end
}