return {
    generator = function(_, cb)
        local ret = {
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
