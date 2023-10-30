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
                name = "Profile main.py",
                builder = function()
                    return {
                        name = "Profile main.py",
                        cmd = "py-spy record -o profile.svg -- python main.py",
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
                name = "Live profile main.py",
                builder = function()
                    return {
                        name = "Live profile main.py",
                        cmd = "py-spy top -- python main.py",
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
                name = "Open profile data",
                builder = function()
                    return {
                        name = "Open profile data",
                        cmd = "browser profile.svg",
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
                        name = "Building HTML Docs",
                        cmd = "sphinx-build -b html docs/source docs/build/html",
                        components = { "default", "unique" }
                    }
                end,
                conditon = { callback = function(opts) return files.exists(files.join(opts.dir, "docs")) end },
                priority = pr(),
            }
        )
        table.insert(
            ret,
            {
                name = "Build Documentation (pdf)",
                builder = function()
                    return {
                        name = "Building PDF Docs",
                        cmd = "make latexpdf",
                        cwd = "docs",
                        components = { "default", "unique" }
                    }
                end,
                conditon = { callback = function(opts) return files.exists(files.join(opts.dir, "docs")) end },
                priority = pr(),
            }
        )
        table.insert(
            ret,
            {
                name = "Open Documentation (html)",
                builder = function()
                    return {
                        name = "Open Docs",
                        cmd = "'/home/oleete/.config/bin/browser' '/home/oleete/Projects/Scintilla/Main/docs/build/html/index.html'",
                        components = { "default", "unique" }
                    }
                end,
                conditon = { callback = function(opts) return files.exists(files.join(opts.dir, "docs")) end },
                priority = pr(),
            }
        )
        table.insert(
            ret,
            {
                name = "Open Documentation (pdf)",
                builder = function()
                    return {
                        name = "Open Docs",
                        cmd = "zathura /home/oleete/Projects/Scintilla/Main/docs/build/latex/scintilla-controller.pdf ",
                        components = { "default", "unique" }
                    }
                end,
                conditon = { callback = function(opts) return files.exists(files.join(opts.dir, "docs")) end },
                priority = pr(),
            }
        )
        table.insert(
            ret,
            {
                name = "Format all",
                builder = function()
                    return {
                        name = "Format all files",
                        cmd = "ruff format --preview scintilla_control/",
                        components = { "default", "unique" }
                    }
                end,
                conditon = {},
                priority = pr(),
            }
        )
        table.insert(
            ret,
            {
                name = "Lint all",
                builder = function()
                    return {
                        name = "Lint all files",
                        cmd = "ruff --preview --watch scintilla_control/",
                        components = { "default", "unique" }
                    }
                end,
                conditon = {},
                priority = pr(),
            }
        )
        table.insert(
            ret,
            {
                name = "Sourcery",
                builder = function()
                    return {
                        name = "Sourcery check",
                        cmd = "sourcery review scintilla_control/",
                        components = { "default", "unique" }
                    }
                end,
                conditon = {},
                priority = pr(),
            }
        )
        table.insert(
            ret,
            {
                name = "Make dependency graph",
                builder = function()
                    return {
                        name = "Make dependency graph",
                        cmd = "pydeps scintilla_control/scintilla_control.py",
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
