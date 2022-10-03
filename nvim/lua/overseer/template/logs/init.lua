return {
    generator = function(_, cb)
        local logHandler = io.popen(
            [[fd -e log]]
        )
        local ret = {}
        if logHandler then
            local logs = logHandler:read("*a")
            logHandler:close()
            for log in logs:gmatch("([^\r\n]+)") do
                table.insert(
                    ret,
                    {
                        name = "Show " .. log,
                        builder = function()
                            return {
                                name = "Show " .. log,
                                cmd = "tail --follow --retry " .. log,
                                components = { "default", "unique" }
                            }
                        end,
                        priority = 150,
                        params = {},
                    }
                )
            end
        end
        cb(ret)
    end
}
