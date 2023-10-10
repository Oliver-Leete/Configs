local files = require("overseer.files")
local TAG = require("overseer.constants").TAG

return {
    condition = {
        callback = function(opts)
            if files.exists(files.join(vim.fn.getcwd(), "platformio.ini")) then
                return true
            end
        end
    },
    generator = function(_, cb)
        local priority = 60
        local pr = function() priority = priority + 1; return priority end
        local ret = {}
        table.insert(
            ret,
            {
                name = "PIO Build",
                builder = function()
                    return {
                        name = "PIO Build",
                        cmd = "platformio run",
                        components = { "default", "unique" },
                    }
                end,
                priority = pr(),
                params = {},
            }
        )
        table.insert(
            ret,
            {
                name = "PIO Clean",
                builder = function()
                    return {
                        name = "PIO Clean",
                        cmd = "platformio run --target clean",
                        components = { "default", "unique" },
                    }
                end,
                priority = pr(),
                params = {},
            }
        )
        table.insert(
            ret,
            {
                name = "PIO Upload",
                builder = function()
                    return {
                        name = "PIO Upload",
                        cmd = "platformio run --target upload",
                        components = { "default", "unique" },
                    }
                end,
                priority = pr(),
                params = {},
            }
        )
        table.insert(
            ret,
            {
                name = "PIO Program",
                builder = function()
                    return {
                        name = "PIO Program",
                        cmd = "platformio run --target program",
                        components = { "default", "unique" },
                    }
                end,
                priority = pr(),
                params = {},
            }
        )
        table.insert(
            ret,
            {
                name = "PIO Test",
                builder = function()
                    return {
                        name = "PIO Test",
                        cmd = "platformio test upload",
                        components = { "default", "unique" },
                    }
                end,
                priority = pr(),
                params = {},
            }
        )
        table.insert(
            ret,
            {
                name = "PIO Remote",
                builder = function()
                    return {
                        name = "PIO Remote",
                        cmd = "platformio remote run --target upload",
                        components = { "default", "unique" },
                    }
                end,
                priority = pr(),
                params = {},
            }
        )
        table.insert(
            ret,
            {
                name = "PIO Make Compile_Command.json",
                builder = function()
                    return {
                        name = "PIO Make Compile_Command.json",
                        cmd = "platformio run --target compiledb",
                        components = { "default", "unique" },
                    }
                end,
                priority = pr(),
                params = {},
            }
        )
        table.insert(
            ret,
            {
                name = "PIO Debug",
                builder = function()
                    return {
                        name = "PIO Debug",
                        cmd = "platformio debug",
                        components = { "default", "unique" },
                    }
                end,
                priority = pr(),
                params = {},
            }
        )

        return cb(ret)
    end,
}
