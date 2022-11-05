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
                        cmd = "python main.py",
                        components = { "default", "unique" }
                    }
                end,
                condition = {
                    callback = function()
                        return files.exists("main.py")
                    end

                },
                priority = 4,
            }
        )
        table.insert(
            ret,
            {
                name = "Run Scintilla",
                builder = function()
                    return {
                        name = "Scintilla",
                        cmd = "python main.py",
                        components = { "default_hide", "unique" }
                    }
                end,
                priority = 1,
                condition = { dir = "/home/oleete/Projects/Scintilla/Main" },
            }
        )
        cb(ret)
    end,
}
