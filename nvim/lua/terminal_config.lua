local Terminal = require("toggleterm.terminal").Terminal
juliaTest = Terminal:new({
    cmd = "juliaTest",
    count = 1,
    on_open = function()
        vim.b[0].my_term_title = "Julia Test"
    end
})
function _juliatest_toggle()
    juliaTest:toggle()
end

function Terminal:send_open(cmd, go_back)
    if not self:is_open() then
        self:open()
    end
    self:send(cmd, go_back)
end


