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

-- Git Signs Settup

require("gitsigns").setup({
    signs = {
        add = { hl = "GitSignsAdd", numhl = "GitSignsAdd", linehl = "GitSignsAdd", text = "▋" },
        change = { hl = "GitSignsChange", numhl = "GitSignsChange", linehl = "GitSignsChange", text = "▋" },
        delete = { hl = "GitSignsDelete", numhl = "GitSignsDelete", linehl = "GitSignsDelete", text = "▂" },
        topdelete = { hl = "GitSignsDelete", numhl = "GitSignsDelete", linehl = "GitSignsDelete", text = "▔" },
        changedelete = { hl = "GitSignsDelete", numhl = "GitSignsDelete", linehl = "GitSignsDelete", text = "▋" },
        empty = {}, -- Unused

        base = nil, -- Use index
        signcolumn = true,
        numhl = true,
        linehl = false,
    },
    --     signs_sec = {
    --         add          = {hl = 'GitSignsAdd'   , numhl='GitSignsAdd'   , linehl='GitSignsAdd'   , text = '▎' },
    --         change       = {hl = 'GitSignsChange', numhl='GitSignsChange', linehl='GitSignsChange', text = '▎' },
    --         delete       = {hl = 'GitSignsDelete', numhl='GitSignsDelete', linehl='GitSignsDelete', text = '_' },
    --         topdelete    = {hl = 'GitSignsDelete', numhl='GitSignsDelete', linehl='GitSignsDelete', text = '‾' },
    --         changedelete = {hl = 'GitSignsDelete', numhl='GitSignsDelete', linehl='GitSignsDelete', text = '▎' },
    --         empty        = {},

    --         base       = nil,
    --         signcolumn = true,
    --         numhl      = true,
    --         linehl     = false,
    --     },
    keymaps = {
        -- Default keymap options
        noremap = true,
        buffer = true,

        -- ["n ]h"] = { expr = true, [[&diff ? "]czz<cmd>let g:dirJumps='h'<cr>m`" : "<cmd>lua require'gitsigns'.next_hunk()<cr>zz<cmd>let g:dirJumps='h'<cr>m'm`"]] },
        -- ["n [h"] = { expr = true, [[&diff ? "[czz<cmd>let g:dirJumps='h'<cr>m`" : "<cmd>lua require'gitsigns'.prev_hunk()<cr>zz<cmd>let g:dirJumps='h'<cr>m'm`"]] },

        -- Text objects
        ["o ih"] = ':<C-U>lua require"gitsigns".select_hunk()<CR>',
        ["x ih"] = ':<C-U>lua require"gitsigns".select_hunk()<CR>',
    },
    watch_index = {
        interval = 1000,
    },
    current_line_blame_position = "eol",
    numhl = true,
    current_line_blame = false,
    sign_priority = 6,
    update_debounce = 100,
    status_formatter = nil,
    use_decoration_api = true,
    use_internal_diff = true,
    -- staged_signs = true,
})
-- DiffView.nvim

local cb = require("diffview.config").diffview_callback
require("diffview").setup({
    diff_binaries = false, -- Show diffs for binaries
    file_panel = {
        width = 35,
        use_icons = true, -- Requires nvim-web-devicons
    },
    key_bindings = {
        -- The `view` bindings are active in the diff buffers, only when the current
        -- tabpage is a Diffview.
        view = {
            ["<tab>"] = cb("select_next_entry"), -- Open the diff for the next file
            ["<s-tab>"] = cb("select_prev_entry"), -- Open the diff for the previous file
            ["<leader>x"] = cb("focus_files"), -- Bring focus to the files panel
            ["<esc>"] = cb("focus_files"),
            -- ["<leader>b"] = cb("toggle_files"),       -- Toggle the files panel.
        },
        file_panel = {
            ["j"] = cb("next_entry"), -- Bring the cursor to the next file entry
            ["<down>"] = cb("next_entry"),
            ["k"] = cb("prev_entry"), -- Bring the cursor to the previous file entry.
            ["<up>"] = cb("prev_entry"),
            ["<cr>"] = cb("select_entry"),
            ["o"] = "<cmd>lua require('diffview').on_keypress('select_entry')<cr><cmd>sleep 100m<cr><cmd>DiffviewToggleFiles<cr>",
            ["p"] = "<cmd>lua require('diffview').on_keypress('select_entry')<cr><cmd>DiffviewFocusFiles<cr>",
            ["<2-LeftMouse>"] = cb("select_entry"),
            ["-"] = cb("toggle_stage_entry"), -- Stage / unstage the selected entry.
            ["S"] = cb("stage_all"), -- Stage all entries.
            ["U"] = cb("unstage_all"), -- Unstage all entries.
            ["XX"] = cb("restore_entry"), -- Restore entry to the state on the left side.
            ["R"] = cb("refresh_files"), -- Update stats and entries in the file list.      ["<tab>"] = cb("select_next_entry"),
            ["<s-tab>"] = cb("select_prev_entry"),
            ["<leader>t"] = cb("focus_files"),
            ["<leader>x"] = cb("toggle_files"),
            ["<esc>"] = "<cmd>DiffviewClose<cr>",
        },
    },
})

-- Neogit Setup

require("neogit").setup({
    disable_signs = false,
    disable_context_highlighting = false,
    -- customize displayed signs
    signs = {
        -- { CLOSED, OPENED }
        section = { ">", "v" },
        item = { ">", "v" },
        hunk = { "", "" },
    },
    integrations = {
        diffview = true,
    },
    -- override/add mappings
    mappings = {
        -- modify status buffer mappings
        status = {
            ["B"] = "BranchPopup",
        },
    },
})

if vim.api.nvim_win_get_option(0, "diff") then
    require("which-key").register({
        ["<leader>"] = {
            ["["] = { "<cmd>diffget LOCAL<cr>", "Take From Local Change" },
            ["]"] = { "<cmd>diffget REMOTE<cr>", "Take From Remote Change" },
            ["<leader>"] = { "<cmd>diffget BASE<cr>", "Take From Base" },
        },
    })
end
