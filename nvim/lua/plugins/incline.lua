return {
    {
        "b0o/incline.nvim",
        config = function()
            local colors = function(props)
                return props.focused
                    and { fg = "#1A1A22", bg = "#7E9CD8", }
                    or { fg = "#1A1A22", bg = "#4A4A60", }
            end

            local file_renderer = function(props)
                local filename = vim.fn.fnamemodify(vim.api.nvim_buf_get_name(props.buf), ":t")
                if filename == "" then
                    filename = "[NA]"
                end
                local ft_icon, _ = MiniIcons.get("file", filename)

                local gui = colors(props)

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

            local is_trouble = function(props) return vim.w[props.win].trouble end

            local trouble_renderer = function(props)
            end

            require("incline").setup({
                window = {
                    zindex = 30,
                    padding = 0,
                    margin = {
                        vertical = { top = vim.o.laststatus == 3 and 0 or 1, bottom = 0 }, -- shift to overlap window borders
                        horizontal = { left = 0, right = 2 },
                    },
                },
                render = function(props)
                    if is_trouble(props) then
                        return trouble_renderer(props)
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
