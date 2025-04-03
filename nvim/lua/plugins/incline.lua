return {
    {
        "b0o/incline.nvim",
        config = function()
            local file_renderer = function(props)
                local filename = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(props.buf), ":t")
                if filename == "" then
                    filename = "[NA]"
                end
                local ft_icon, _ = MiniIcons.get("file", filename)

                local gui = require("user.colors").colors(props)

                return {
                    { "", guifg = gui.bg, guibg = gui.fg },
                    { filename, gui = "bold" },
                    " ",
                    { ft_icon, " " },
                    { "", guifg = gui.bg, guibg = gui.fg },
                    guifg = gui.fg,
                    guibg = gui.bg,
                }
            end

            require("incline").setup({
                window = {
                    zindex = 30,
                    padding = 0,
                    margin = {
                        vertical = { top = vim.o.laststatus == 3 and 0 or 1, bottom = 0 }, -- snift to overlap window borders
                        horizontal = { left = 0, right = 2 },
                    },
                },
                ignore = {
                    buftypes = {},
                    filetypes = {},
                    unlisted_buffers = false,
                },
                render = function(props)
                    if vim.b[props.buf].incline_renderer ~= nil then
                        return vim.b[props.buf].incline_renderer(props)
                    else
                        return file_renderer(props)
                    end
                end,
            })
        end,
        dependencies = {
            { "echasnovski/mini.nvim" },
        },
    },
}
