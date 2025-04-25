---@module "lazy"
---@type LazySpec
return {
    {
        "akinsho/toggleterm.nvim",
        dev = true,
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
            vim.g.repl_by_ft = vim.tbl_map(
                function(repl) return vim.tbl_deep_extend("keep", repl, { trim_spaces = true }) end,
                opts.ft_repls
            )
            opts.ft_repls = nil
            require("toggleterm").setup(opts)
        end,
        keys = {
            { "<leader>tt", "<cmd>ToggleTermLast direction=horizontal<cr>", desc = "Toggle Terminal" },
            { "<leader>ts", "<cmd>TermSelect<cr>", desc = "Select Terminal" },
            { "<leader>tn", "<cmd>TermNew<cr>", desc = "New Terminal" },
        },
    },
}
