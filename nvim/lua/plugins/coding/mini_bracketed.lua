return {
    "echasnovski/mini.bracketed",
    opts = {
        buffer     = { suffix = "", options = {} },
        comment    = { suffix = "", options = {} },
        conflict   = { suffix = "", options = {} },
        diagnostic = { suffix = "", options = {} },
        file       = { suffix = "", options = {} },
        indent     = { suffix = "", options = {} },
        jump       = { suffix = "", options = {} },
        location   = { suffix = "", options = {} },
        oldfile    = { suffix = "", options = {} },
        quickfix   = { suffix = "", options = {} },
        treesitter = { suffix = "", options = {} },
        undo       = { suffix = "", options = {} },
        window     = { suffix = "", options = {} },
        yank       = { suffix = "", options = {} },
    },
    keys = {
        {
            "[c",
            function() require("user.targets").func(require("mini.bracketed").comment, "c", "backward") end,
            mode = { "n", "x", "o" },
            desc = "Comment",
        },
        {
            "]c",
            function() require("user.targets").func(require("mini.bracketed").comment, "c", "forward") end,
            mode = { "n", "x", "o" },
            desc = "Comment",
        },

        {
            "[j",
            function() require("user.targets").func(require("mini.bracketed").jump, "j", "backward") end,
            mode = { "n", "x", "o" },
            desc = "Jump",
        },
        {
            "]j",
            function() require("user.targets").func(require("mini.bracketed").jump, "j", "forward") end,
            mode = { "n", "x", "o" },
            desc = "Jump",
        },
    }
}
