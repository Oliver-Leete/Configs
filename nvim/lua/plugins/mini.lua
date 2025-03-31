local mini_setup = function()
    vim.g.miniindentscope_disable = true

    vim.keymap.set("x", ',mj', function() require('mini.move').move_selection("down") end, { nowait = true })
    vim.keymap.set("x", ',mh', function() require('mini.move').move_selection("left") end, { nowait = true })
    vim.keymap.set("x", ',mk', function() require('mini.move').move_selection("up") end, { nowait = true })
    vim.keymap.set("x", ',ml', function() require('mini.move').move_selection("right") end, { nowait = true })
    vim.keymap.set("n", ',mh', function() require('mini.move').move_line("left") end, { nowait = true })
    vim.keymap.set("n", ',mj', function() require('mini.move').move_line("down") end, { nowait = true })
    vim.keymap.set("n", ',mk', function() require('mini.move').move_line("up") end, { nowait = true })
    vim.keymap.set("n", ',ml', function() require('mini.move').move_line("right") end, { nowait = true })

    require("mini.indentscope").setup({
        mappings = {
            object_scope = "",
            object_scope_with_border = "",
            goto_top = "",
            goto_bottom = "",

        },
        symbol = '▎"',
    })

    require("mini.comment").setup({
        mappings = {
            comment = ",c",
            comment_line = ",cc",
            textobject = "ic",
        },
    })

    require("mini.icons").setup()

    require("mini.pairs").setup()

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
                input = ts_input({ outer = "@call.outer", inner = "@call.inner" })
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

    require("mini.misc").setup({
        make_global = { "put_text", "zoom" },
    })

    require("mini.misc").setup_restore_cursor({
        center = true
    })

    require("mini.align").setup({
        mappings = {
            start = "",
            start_with_preview = ",t",
        }
    })

    local splitjoin = require("mini.splitjoin")
    splitjoin.setup({
        mappings = {
            toggle = ",j",
        },
        detect = {
            separator = "[,;]",
        },
        split = {
            hooks_pre = {},
            hooks_post = {},
        },
        -- Join options
        join = {
            hooks_pre = {},
            hooks_post = {},
        },
    })


    local hipatterns = require("mini.hipatterns")
    hipatterns.setup({
        highlighters = {
            hex_color = hipatterns.gen_highlighter.hex_color(),
        },
    })

    require("mini.operators").setup({
        evaluate = { prefix = ",=" },
        exchange = {
            prefix = "$",
            reindent_linewise = false,
        },
        multiply = {
            prefix = "+"
        },
        replace = {
            prefix = "R",
            reindent_linewise = false,
        },
        sort = {
            prefix = ",s",
        },
    })

    local diff = require("mini.diff")
    diff.setup({
        view = { style = "number" },
        mappings = {
            apply = "<leader>gs",
            reset = "<leader>gr",
            textobject = "",
            goto_first = "",
            goto_prev = "",
            goto_next = "",
            goto_last = "",
        },
    })
    vim.keymap.set("n", "<leader>gS", function() diff.do_hunks(0, "apply") end, { desc = "Apply all hunks" })
    vim.keymap.set("n", "<leader>gR", function() diff.do_hunks(0, "reset") end, { desc = "Reset all hunks" })

    require("mini.git").setup({
        command = {
            split = "vertical",
        }
    })

    require("mini.notify").setup({
        content = {
            format = function(notif) return notif.msg end,
        },
        window = {
            config = function()
                local has_tabline = vim.o.showtabline == 2 or
                    (vim.o.showtabline == 1 and #vim.api.nvim_list_tabpages() > 1)
                local has_statusline = vim.o.laststatus > 0
                local max_height = vim.o.lines - vim.o.cmdheight - (has_tabline and 1 or 0) - (has_statusline and 1 or 0)
                local max_width = vim.o.columns
                return {
                    border = require("user.settings").border,
                    relative = "editor",
                    anchor = "SE",
                    col = max_width,
                    row = max_height,
                }
            end
        }
    })
    vim.notify = require("mini.notify").make_notify()
end

return {
    "echasnovski/mini.nvim",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
    config = mini_setup,
}
