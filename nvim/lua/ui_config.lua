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
        width = 85,
        height = 1,
        options = {
            signcolumn = "no",
            number = false, -- disable number column
            relativenumber = false, -- disable relative numbers
            scrolloff = 999,
        },
    },
    plugins = {
        gitsigns = true, -- disables git signs
    },
    on_open = function()
        vim.api.nvim_command("IndentBlanklineDisable")
        vim.g.diagnostics_active = true
        toggle_diagnostics()
    end,
    on_close = function()
        vim.api.nvim_command("IndentBlanklineEnable")
        vim.g.diagnostics_active = false
        toggle_diagnostics()
    end,
})
require("twilight").setup({
    dimming = {
        alpha = 0.25,
    },
    context = 10,
    exclude = {},
})

-- BufferLine

require("bufferline").setup({
    options = {
        view = "multiwindow",
        numbers = "none",
        number_style = "none",
        mappings = false,
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

-- LuaLine Status Line
local function toggleTermNum()
    return vim.api.nvim_eval('&ft == "toggleterm" ? "TERM ".b:toggle_number."" : ""')
end

require("lualine").setup({
    options = {
        theme = "tokyonight",
        section_separators = { "", "" },
        component_separators = { "", "" },
        extensions = { "nvim-tree" },
    },
    sections = {
        lualine_a = { { "mode" } },
        lualine_b = { { toggleTermNum }, { "branch" }, { "filename", file_status = true } },
        lualine_c = {
            {
                "diagnostics",
                sources = { "nvim_lsp" },
                sections = { "error", "warn", "info" },
                color_error = "#db4b4b",
                color_warn = "#e0af68",
                color_info = "#1abc9c",
                { error = "", warn = "", info = "" },
            },
        },
        lualine_x = {
            {
                "diff",
                color_added = "#266d6a",
                color_modified = "#536c9e",
                color_removed = "#b2555b",
                symbols = { added = "+", modified = "~", removed = "-" },
            },
            "fileformat",
            "filetype",
        },
        lualine_y = { "progress" },
        lualine_z = { "location" },
    },
    inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {},
    },
})

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
