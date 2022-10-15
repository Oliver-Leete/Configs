local rt = require("rust-tools")

local function getCommand(result)
    local ret = {}
    local args = result.args

    local dir = args.workspaceRoot

    ret = vim.list_extend(ret, args.cargoArgs or {})
    ret = vim.list_extend(ret, args.cargoExtraArgs or {})
    table.insert(ret, "--")
    ret = vim.list_extend(ret, args.executableArgs or {})

    return "cargo", ret, dir
end

return {
    generator = function(_, cb)
        local handler = function(_, results)
            if results == nil then return end
            local ret = {}
            for _, result in pairs(results) do
                local command, args, cwd = getCommand(result)
                table.insert(
                    ret,
                    {
                        name = result.label,
                        builder = function()
                            return {
                                name = result.label,
                                cmd = command,
                                cwd = cwd,
                                args = args,
                                components = { "default", "unique" }
                            }
                        end,
                    }
                )
            end
            cb(ret)
        end

        rt.utils.request(0, "experimental/runnables", {
            textDocument = vim.lsp.util.make_text_document_params(0),
            position = nil, -- get em all
        }, handler)
    end,
    condition = {
        callback = function()
            return #vim.lsp.get_active_clients({ name = "rust_analyzer" }) >= 1
        end
    }
}
