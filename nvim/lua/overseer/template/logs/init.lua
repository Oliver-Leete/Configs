return {
    generator = function(_, cb)
        local ret = {}
            local logs = vim.fn.systemlist([[fd -e log]])
            for _, log in pairs(logs) do
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
        cb(ret)
    end
}
