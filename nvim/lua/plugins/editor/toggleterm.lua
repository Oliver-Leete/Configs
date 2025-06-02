---@module "lazy"
---@type LazySpec
return {
    {
        "akinsho/toggleterm.nvim",
        dev = false,
        cmd = {
            "TermSelect",
            "TermExec",
            "TermNew",
            "ToggleTerm",
            "ToggleTermLast",
            "ToggleTermToggleAll",
            "ToggleTermSendVisualLines",
            "ToggleTermSendVisualSelection",
            "ToggleTermSendCurrentLine",
            "ToggleTermSetName",
        },
        opts_extend = { "ft_repls" },
        opts = {
            shell = "fish",
            shade_terminals = false,
            float_opts = {
                border = vim.o.winborder,
            },
            responsiveness = {
                horizontal_breakpoint = 135,
            },
        },
        config = function(_, opts)
            local esc_timer = (vim.uv or vim.loop).new_timer()
            vim.api.nvim_create_autocmd("TermOpen", {
                pattern = "term://*",
                callback = function(ev)
                    local term = require("toggleterm.terminal").find(function(t) return t.bufnr == ev.buf end)
                    if not term then return end
                    vim.keymap.set({ "n", "t" }, "<c-f>", function()
                        if term.direction and term.direction ~= "float" then
                            term:close()
                            term:change_direction("float")
                            term:open()
                        else
                            term:close()
                            term:change_direction("horizontal")
                            term:open()
                        end
                    end, { buffer = term.bufnr, desc = "Toggle terminal floating" })
                    vim.keymap.set(
                        { "n" },
                        "<esc>",
                        function() term:close() end,
                        { buffer = term.bufnr, desc = "Close terminal" }
                    )
                    vim.keymap.set({ "t" }, "<esc>", function()
                        if esc_timer:is_active() then
                            esc_timer:stop()
                            vim.cmd("stopinsert")
                        else
                            esc_timer:start(200, 0, function() end)
                            return "<esc>"
                        end
                    end, { buffer = term.bufnr, expr = true, desc = "Double escape to normal mode" })
                end,
            })

            vim.g.repl_by_ft = vim.tbl_map(
                function(repl) return vim.tbl_deep_extend("keep", repl, { trim_spaces = true }) end,
                opts.ft_repls
            )
            opts.ft_repls = nil
            require("toggleterm").setup(opts)
        end,
        keys = {
            { "<leader>tt", "<cmd>ToggleTerm direction=horizontal<cr>", desc = "Toggle terminal" },
            { "<leader>tT", "<cmd>ToggleTermToggleAll<cr>", desc = "Toggle all terminals" },
            { "<leader>ts", "<cmd>TermSelect<cr>", desc = "Select Terminal" },
            { "<leader>tn", "<cmd>TermNew<cr>", desc = "New Terminal" },
        },
    },
}
