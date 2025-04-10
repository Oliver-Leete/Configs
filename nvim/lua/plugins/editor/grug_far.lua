---@module "lazy"
---@type LazySpec
return {
    "MagicDuck/grug-far.nvim",
    opts = {
        headerMaxWidth = 80,
        enabledReplacementInterpreters = { "default", "lua" },
        transient = true,
        keymaps = {
            qflist = { n = "<localleader>l" },
            replace = { n = "<localleader>r" },
            syncLocations = { n = "<localleader>s" },
            syncLine = { n = "<localleader>S" },
            close = { n = "<localleader>c" },
            historyOpen = { n = "<localleader>h" },
            historyAdd = { n = "<localleader>a" },
            refresh = { n = "<localleader>f" },
            openLocation = { n = "<localleader>o" },
            openNextLocation = { n = "<down>" },
            openPrevLocation = { n = "<up>" },
            gotoLocation = { n = "<enter>" },
            pickHistoryEntry = { n = "<enter>" },
            abort = { n = "<localleader>b" },
            help = { n = "g?" },
            toggleShowCommand = { n = "<localleader>w" },
            swapEngine = { n = "<localleader>e" },
            previewLocation = { n = "<localleader>i" },
            swapReplacementInterpreter = { n = "<localleader>x" },
            applyNext = { n = "<localleader>j" },
            applyPrev = { n = "<localleader>k" },
            syncNext = { n = "<localleader>n" },
            syncPrev = { n = "<localleader>p" },
            syncFile = { n = "<localleader>v" },
            nextInput = { n = "<tab>" },
            prevInput = { n = "<s-tab>" },
        },
    },
    cmd = "GrugFar",
    keys = {
        {
            "<leader>sr",
            function() require("grug-far").open() end,
            desc = "Search and Replace",
        },
        {
            "<leader>sR",
            function() require("grug-far").open({ prefills = { paths = vim.fn.expand("%") } }) end,
            desc = "Search and Replace in file"
        },
        {
            "<leader>sr",
            function()
                require("grug-far").open({ visualSelectionUsage = "operate-within-range" })
            end,
            mode = { "x" },
            desc = "Search within range"
        },
    },
}
