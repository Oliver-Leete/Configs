OvTermNum = 0
return {
    generator = function(_, cb)
        ret = {
            {
                name = "View Animation",
                builder = function()
                    return {
                        name = "Animation",
                        cmd = "mpv --loop-file=inf /tmp/fig.gif",
                        components = { "unique", { "on_complete_dispose", timeout = 1 } }
                    }
                end,
                priority = 151,
                condition = {
                    dir = "/home/oleete/Projects/PowderModel"
                },
            },
            {
                name = "System Info (btop)",
                builder = function()
                    return {
                        name = "btop",
                        cmd = "btop",
                        components = { "def_dispose", "unique" }
                    }
                end,
                priority = 155,
                params = {},
            },
            {
                name = "Lazygit",
                builder = function()
                    return {
                        name = "lazygit",
                        cmd = "lazygit",
                        components = { "def_dispose", "unique" }
                    }
                end,
                priority = 2,
                params = {},
            },
            {
                name = "Build Document",
                builder = function()
                    return {
                        name = "Build Document",
                        cmd = "latexmk -pdf -file-line-error -synctex=1 OML-Thesis.tex",
                        components = { "default_hide", 'unique' }
                    }
                end,
                priority = 5,
                condition = {
                    dir = "/home/oleete/UniversityDrive/Thesis/thesis"
                }
            },
            {
                name = "Fish",
                builder = function()
                    OvTermNum = OvTermNum + 1
                    return {
                        name = "Fish " .. OvTermNum,
                        cmd = "fish",
                        components = { "def_dispose" }
                    }
                end,
                priority = 1,
                params = {},
            },
            {
                name = "Make",
                builder = function()
                    return {
                        name = "Make",
                        cmd = vim.split(vim.o.makeprg, "%s+"),
                    }
                end,
                priority = 1000,
                condition = {
                    -- Only show if there is a more interesting make program, for make itself use the overseer built
                    -- in command.
                    callback = function()
                        return vim.go.makeprg ~= "make"
                    end
                }
            },
        }
        cb(ret)
    end
}
