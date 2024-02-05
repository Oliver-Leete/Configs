require("neotest").setup({
    consumers = {
        overseer = require("neotest.consumers.overseer"),
    },
    overseer = {
        enabled = true,
        force_default = true,
    },
    adapters = {
        require('rustaceanvim.neotest'),
        require("neotest-python")({
            python = "venv/bin/python",
            is_test_file = function(file_path)
                local ending = ".py"
                return file_path:sub(- #ending) == ending
            end
        }),
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
    summary = {
        open = "topleft vsplit | vertical resize 50",
        mappings = {
            attach = "a",
            clear_marked = "M",
            clear_target = "T",
            debug = "d",
            debug_marked = "D",
            expand = { "<CR>", "<2-LeftMouse>" },
            expand_all = "e",
            jumpto = "i",
            mark = "m",
            next_failed = "J",
            output = "p",
            prev_failed = "K",
            run = "r",
            run_marked = "R",
            short = "P",
            stop = "u",
            target = "t"
        },
    }
})
