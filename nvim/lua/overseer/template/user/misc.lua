return {
    generator = function(_, cb)
        ret = {
            {
                name = "Edit Directory",
                builder = function()
                    return {
                        name = "edir",
                        cmd = "fish -c 'fd -HL | edir -Z'",
                    }
                end,
                priority = 5,
                params = {}
            },
            {
                name = "Build Document",
                builder = function()
                    return {
                        name = "Build Document",
                        cmd = "latexmk -synctex=1 -f -silent",
                        components = { "default", 'unique' }
                    }
                end,
                priority = 5,
                condition = {
                    dir = "/home/oleete/UniversityDrive/Thesis/thesis"
                }
            },
            {
                name = "Verbose Build Document",
                builder = function()
                    return {
                        name = "Build Document",
                        cmd = "latexmk -synctex=1 -f",
                        components = { "default", "unique",
                            {
                                "user.start_open",
                                goto_prev = true,
                            },
                        },
                    }
                end,
                priority = 5,
                condition = {
                    dir = "/home/oleete/UniversityDrive/Thesis/thesis"
                }
            },
            {
                name = "Clean Build Files",
                builder = function()
                    return {
                        name = "Clean Build Files",
                        cmd = "latexmk -c",
                        components = { "default", 'unique' }
                    }
                end,
                priority = 5,
                condition = {
                    dir = "/home/oleete/UniversityDrive/Thesis/thesis"
                }
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
