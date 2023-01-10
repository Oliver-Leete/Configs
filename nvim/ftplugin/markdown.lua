local peek = require("peek")
peek.setup({})
Map("n", "<localleader><localleader>", function()
    if not peek.is_open() then
        peek.open()
    else
        peek.close()
    end
end, { buffer = 0 })


vim.b.textwidth = 80
vim.bo.textwidth = 80
