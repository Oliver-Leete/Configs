require("neotest").setup({
    consumers = {
        overseer = require("neotest.consumers.overseer"),
    },
    overseer = {
        enabled = true,
        force_default = true,
    },
    adapters = {
        require("neotest-rust"),
        require("neotest-python")({
            python = "venv/bin/python",
            is_test_file = function(file_path)
                local ending = ".py"
                return file_path:sub(- #ending) == ending
            end
        }),
        require("neotest-haskell"),
        require("neotest-plenary"),
        require("neotest.adapters.neotest-julia-testitem"),
        require("neotest.adapters.neotest-julia-benchmarktools")
    },
    floating = {
        border = Border,
        max_height = 0.9,
        max_width = 0.9,
    },
    output_panel = {
        enabled = true,
        open = "",
    },
    icons = {
        running_animated = { "⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷" },
        passed = " ",
        running = " ",
        failed = " ",
        skipped = " ",
        unknown = "?",
        non_collapsible = "─",
        collapsed = "─",
        expanded = "╮",
        child_prefix = "├",
        final_child_prefix = "╰",
        child_indent = "│",
        final_child_indent = " ",
    },
    mappings = {
        expand = { "<CR>", "<2-LeftMouse>" },
        expand_all = "e",
        output = "p",
        short = "P",
        attach = "a",
        jumpto = "i",
        stop = "u",
        run = "r",
        mark = "m",
        run_marked = "R",
        clear_marked = "M",
        target = "t",
        clear_target = "T",
    },

})
