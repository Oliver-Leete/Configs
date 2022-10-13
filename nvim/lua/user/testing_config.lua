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
        require("neotest-python"),
        require("neotest-haskell"),
        require("neotest.adapters.neotest-julia-testitem"),
        require("neotest.adapters.neotest-julia-benchmarktools")
    },
    floating = {
        border = Border,
        max_height = 0.9,
        max_width = 0.9,
        options = {},
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

