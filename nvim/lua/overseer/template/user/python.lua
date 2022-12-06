local files = require("overseer.files")

return {
    condition = {
        dir = "/home/oleete/Projects/Scintilla/Main"
    },
    generator = function(_, cb)
        local ret = {}
        local priority = 60
        local pr = function() priority = priority + 1; return priority end

        table.insert(
            ret,
            {
                name = "Run main.py",
                builder = function()
                    return {
                        name = "Running main.py",
                        cmd = "python main.py",
                        strategy = {"toggleterm", open_on_start = false},
                        components = { "default", "unique" }
                    }
                end,
                condition = {
                    callback = function()
                        return files.exists("main.py")
                    end

                },
                priority = 1,
            }
        )
        table.insert(
            ret,
            {
                name = "Build Documentation (html)",
                builder = function()
                    return {
                        name = "Building Docs",
                        strategy = {"toggleterm", open_on_start = false},
                        cmd = "sphinx-build -b html docs/source docs/build/html",
                        components = { "default", "unique" }
                    }
                end,
                conditon = { callback = function(opts) return files.exists(files.join(opts.dir, "docs")) end },
                priority = pr(),
            }
        )
        cb(ret)
    end,
}
