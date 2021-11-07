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

-- Telescope Setup

require("telescope").load_extension("bibtex")
require("telescope").load_extension("gh")
require("telescope").load_extension("media_files")
-- require("telescope").load_extension("session-lens")
require("telescope").load_extension("heading")

local actions = require("telescope.actions")
local extensions = require("telescope").extensions

require("telescope").setup({
    defaults = {
        vimgrep_arguments = {
            "rg",
            "--color=never",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
            "--smart-case",
        },
        prompt_prefix = "> ",
        selection_caret = "> ",
        entry_prefix = "  ",
        initial_mode = "insert",
        selection_strategy = "reset",
        sorting_strategy = "descending",
        layout_strategy = "vertical",
        layout_config = {
            vertical = {
                width = 100,
                preview_height = 40,
                height = 80,
                mirror = false,
            },
        },
        file_sorter = require("telescope.sorters").get_fuzzy_file,
        file_ignore_patterns = {},
        generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
        path_display = { "shorten" },
        winblend = 0,
        border = {},
        borderchars = {
            { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
            results = { "─", "│", " ", "│", "┌", "┐", "│", "│" },
            prompt = { "─", "│", "─", "│", "├", "┤", "┘", "└" },
            preview = { "─", "│", "─", "│", "┌", "┐", "┘", "└" },
        },
        color_devicons = true,
        use_less = true,
        set_env = { ["COLORTERM"] = "truecolor" }, -- default = nil,
        file_previewer = require("telescope.previewers").vim_buffer_cat.new,
        grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
        qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
        pickers = {
            buffers = {
                show_all_buffers = true,
                sort_mru = true,
            },
            grep_string = {
                use_regex = true,
            },
            -- lsp_definitions = {
            --     jump_type = "vsplit",
            -- },
            -- lsp_implementations = {
            --     jump_type = "vsplit",
            -- },
        },
        mappings = {
            i = {
                ["<c-u>"] = false,
                ["<c-a>"] = { "<home>", type = "command" },
                ["<c-e>"] = { "<end>", type = "command" },
                ["<C-q>"] = actions.smart_send_to_qflist,
                ["<C-Q>"] = actions.smart_add_to_qflist,
                ["<C-l>"] = actions.smart_send_to_loclist,
                ["<C-L>"] = actions.smart_add_to_loclist,
                ["<C-n>"] = actions.cycle_history_next,
                ["<C-p>"] = actions.cycle_history_prev,
                ["<C-space>"] = actions.toggle_selection + actions.move_selection_worse,
                ["<C-j>"] = actions.move_to_top,
                ["<C-h>"] = actions.move_to_middle,
                ["<C-k>"] = actions.move_to_bottom,
                ["<C-s>"] = extensions.hop.hop,
                ["<C-S>"] = extensions.hop.hop_toggle_selection,
            },
            n = {
                ["<C-q>"] = actions.smart_send_to_qflist,
                ["<C-Q>"] = actions.smart_add_to_qflist,
                ["<C-l>"] = actions.smart_send_to_loclist,
                ["<C-L>"] = actions.smart_add_to_loclist,
                ["<C-n>"] = actions.cycle_history_next,
                ["<C-p>"] = actions.cycle_history_prev,
                ["<C-space>"] = actions.toggle_selection + actions.move_selection_worse,
                ["<C-j>"] = actions.move_to_top,
                ["<C-h>"] = actions.move_to_middle,
                ["<C-k>"] = actions.move_to_bottom,
                ["<C-s>"] = extensions.hop.hop,
                ["<C-S>"] = extensions.hop.hop_toggle_selection,
            },
        },
    },
    extensions = {
        bibtex = {
            depth = 2,
            global_files = { "/home/oleete/UniDrive/1_Thesis/0.1_LaTeX/Citations.bib" },
        },
        fzf = {
            override_generic_sorter = true, -- override the generic sorter
            override_file_sorter = true, -- override the file sorter
            case_mode = "smart_case", -- or "ignore_case" or "respect_case"
        },
        hop = {
            keys = { "t", "n", "s", "e", "r", "i", "a", "o", "d", "h", "g", "j", "p", "l", "f", "u", "w", "y" },
            sign_hl = { "WarningMsg", "Title" },
            line_hl = { "CursorLine", "Noraml" },
            clear_selection_hl = true,
            trace_entry = true,
            reset_selection = true,
        },
        media_files = {},
    },
})

require("telescope").load_extension("hop")


require("telescope").load_extension("fzf")
require("telescope").load_extension("bibtex")

