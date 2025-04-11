local func_out = function()
    local fun_name = MiniSurround.user_input('Function name')
    if fun_name == nil then return nil end
    return { left = ('%s('):format(fun_name), right = ')' }
end
---@module "lazy"
---@type LazySpec
return {
    {
        "echasnovski/mini.surround",
        config = function()
            local ts_input = require("mini.surround").gen_spec.input.treesitter
            require("mini.surround").setup({
                mappings = {
                    add = "yp",
                    visual_add = "P",
                    delete = "dp",
                    find = "gp",
                    find_left = "gP",
                    replace = "cp",
                    update_n_lines = "",
                    highlight = "",
                },
                n_lines = 200,
                search_method = "cover_or_nearest",
                custom_surroundings = {
                    f = {
                        input = { '%f[%w_][%w_]+%b()', '^.-%(().*()%)$' },
                        output = func_out,
                    },
                    F = {
                        input = ts_input({
                            outer = "@call.outer",
                            inner = "@call.inner",
                        }),
                        output = func_out,
                    },
                    o = {
                        input = ts_input({
                            outer = "@block.outer",
                            inner = "@block.inner",
                        }),
                    },
                    s = {
                        input = ts_input({
                            outer = "@function.outer",
                            inner = "@function.inner",
                        }),
                    },
                    k = {
                        input = ts_input({
                            outer = "@key.outer",
                            inner = "@key.inner",
                        }),
                    },
                    v = {
                        input = ts_input({
                            outer = "@value.outer",
                            inner = "@value.inner",
                        }),
                    },
                }
            })
            vim.keymap.del("x", "yp")
        end
    },
    {
        "RRethy/nvim-treesitter-endwise",
    },
}
