---@module "lazy"
---@type LazySpec
return {
    "echasnovski/mini.hipatterns",
    opts = function()
        return {
            highlighters = {
                hex_color = require("mini.hipatterns").gen_highlighter.hex_color(),
            },
        }
    end
}
