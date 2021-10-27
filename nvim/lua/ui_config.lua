----------------------------------------------------------------------------------------------------
--                      _   _   ______    ____   __      __  _____   __  __                       --
--                     | \ | | |  ____|  / __ \  \ \    / / |_   _| |  \/  |                      --
--                     |  \| | | |__    | |  | |  \ \  / /    | |   | \  / |                      --
--                     | . ` | |  __|   | |  | |   \ \/ /     | |   | |\/| |                      --
--                     | |\  | | |____  | |__| |    \  /     _| |_  | |  | |                      --
--                     |_| \_| |______|  \____/      \/     |_____| |_|  |_|                      --
--                                                                                                --
----------------------------------------------------------------------------------------------------
-- Oliver Leete <oliverleete@gmail.com>                                                            --
-- https://github.com/oliver-leete                                                                 --
----------------------------------------------------------------------------------------------------

-- Zen Mode

require("zen-mode").setup({
    window = {
        backdrop = 0.9, -- shade the backdrop of the Zen window. Set to 1 to keep the same as Normal
        width = 105,
        height = 1,
        options = {
            signcolumn = "no",
            number = false, -- disable number column
            relativenumber = false, -- disable relative numbers
            -- scrolloff = 999,
            wrap = true,
        },
    },
    plugins = {
        gitsigns = true, -- disables git signs
        options = {
            enabled = true,
            ruler = true,
            showcmd = true,
        }
    },
    on_open = function()
        vim.cmd("IndentBlanklineDisable")
        vim.cmd("WindLineFloatToggle")
        vim.cmd([[augroup DeclutterSleepyWins
                    autocmd!
                augroup END]])
    end,
    on_close = function()
        vim.cmd("IndentBlanklineEnable")
        vim.cmd("WindLineFloatToggle")
        vim.cmd([[augroup DeclutterSleepyWins
                    autocmd!
                    autocmd BufEnter,FocusGained,InsertLeave,WinEnter * setlocal cursorline
                    autocmd BufLeave,FocusLost,InsertEnter,WinLeave * setlocal nocursorline
                    autocmd BufEnter,FocusGained,WinEnter * setlocal signcolumn=yes:2
                    autocmd BufLeave,FocusLost,WinLeave * setlocal signcolumn=no
                augroup END
                set signcolumn=yes:2]])
    end,
})

-- BufferLine

require("bufferline").setup({
    options = {
        view = "multiwindow",
        numbers = "none",
        buffer_close_icon = "",
        modified_icon = "●",
        close_icon = "",
        left_trunc_marker = "",
        right_trunc_marker = "",
        max_name_length = 18,
        max_prefix_length = 15,
        tab_size = 18,
        diagnostics = "nvim_lsp",
        diagnostics_indicator = function(count, level)
            local icon = level:match("error") and "" or (level:match("warning") and "" or "")
            return " " .. icon .. count
        end,
        show_buffer_close_icons = true,
        show_close_icon = true,
        show_tab_indicators = true,
        persist_buffer_sort = true,
        separator_style = "thick",
        enforce_regular_tabs = true,
        always_show_bufferline = true,
        offsets = {
            { filetype = "NvimTree", text = "File Explorer", text_align = "center" },
            { filetype = "DiffviewFiles", text = "Git Changes", text_align = "center" },
            { filetype = "help", text = "Help", text_align = "center" },
            { filetype = "Outline", text = "Symbols", text_align = "center" },
            { filetype = "vim-plug", text = "Plugins", text_align = "center" },
            { filetype = "undotree", text = "Undo Tree", text_align = "center" },
        },
    },
})

-- Windline Status Line

-- require("wlsample.bubble")

vim.cmd("WindLineFloatToggle")

-- Indent Blankline Settings
require("indent_blankline").setup({
    char = "│",
    use_treesitter = true,
    show_current_context = true,
    show_first_indent_level = false,
    indent_level = 6,
    filetype_exclude = { "qf", "outline", "vimplug", "undotree", "help", "DiffviewFiles", "juliadoc" },
    buftype_exclude = { "teriminal" },
})
