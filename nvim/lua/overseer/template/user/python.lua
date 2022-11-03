local files = require("overseer.files")

return {
    generator = function(_, cb)
        local ret = {}
        table.insert(
            ret,
            {
                name = "Run main.py",
                builder = function()
                    return {
                        name = "Running main.py",
                        cmd =  "python main.py",
                    }
                end,
                condition = {
                    callback = function ()
                        return files.exists("main.py")
                    end

                },
                priority = 4,
            }
        )
        cb(ret)
    end,
}
