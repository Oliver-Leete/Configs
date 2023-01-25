require("noice").setup({
    messages = {
        enabled = true,
        view = "mini",
        view_error = "mini",
        view_warn = "mini",
        view_history = "split",
        view_search = "virtualtext",
    },
    popupmenu = {
        enabled = true,
        backend = "cmp",
    },
    command = {
        history = {
            view = "split",
        },
    },
    notify = {
        enabled = true,
        view = "notify",
    },
    routes = {
        {
            view = "split",
            filter = { event = "msg_show", min_width = 100, min_height = 20 },
        },
        {
            view = false,
            filter = { any = {
                { event = "msg_show", find = "; before #" },
                { event = "msg_show", find = "; after #" },
            } },
        },
        {
            filter = {
                event = "msg_show",
                kind = "",
                find = "written",
            },
            opts = { skip = true },
        },
    },
    lsp = {
        hover = {
            enabled = true,
        },
        signature = {
            enabled = true,
        },
        progress = {
            enabled = true,
            view = "mini",
        },
        message = {
            enabled = true,
            view = "mini",
        },
        documentation = {
            view = "hover",
            opts = {
                lang = "markdown",
                replace = true,
                render = "plain",
                format = { "{message}" },
                win_options = { winblend = 0, concealcursor = "n", conceallevel = 3 },
            },
        },
        override = {
            ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
            ["vim.lsp.util.stylize_markdown"] = true,
            ["cmp.entry.get_documentation"] = true,
        },
    },
    health = {
        checker = true,
    },
    presets = {
        long_message_to_split = true,
        -- command_palette = false,
        lsp_doc_border = true,
        -- inc_rename = true,
    },
    views = {
        split = {
            enter = true,
            position = "top",
            win_options = {
                winhighlight = {
                    Normal = "Normal",
                },
            }
            ,
        }
    },
})
