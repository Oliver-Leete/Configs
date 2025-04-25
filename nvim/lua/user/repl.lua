local M = {}

M.send_line = function()
    local ft = require("user.utils").get_pos_lang()
    if not ft then return end

    local repl = vim.g.repl_by_ft[ft]
    if not repl then return end

    local term = require("toggleterm.terminal").find(function(t) return t.display_name == ft end)
    if not term then
        term = require("toggleterm.terminal").Terminal:new({
            display_name = ft,
            cmd = table.concat(repl.cmd, " "),
        })
    end
    term:open()
    term:close()
    dd(term)
    require("toggleterm").send_lines_to_terminal("single_line", repl.trim_spaces, { args = term.id })
end

return M
