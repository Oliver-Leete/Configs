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

local action_state = require("telescope.actions.state")
local actions = require("telescope.actions")
local from_entry = require("telescope.from_entry")
local entry_to_qf = function(entry)
    local text = entry.text

    if not text then
        if type(entry.value) == "table" then
            text = entry.value.text
        else
            text = entry.value
        end
    end

    return {
        bufnr = entry.bufnr,
        filename = from_entry.path(entry, false),
        lnum = vim.F.if_nil(entry.lnum, 1),
        col = vim.F.if_nil(entry.col, 1),
        text = text,
    }
end

local openAndList = function(prompt_bufnr)
    local picker = action_state.get_current_picker(prompt_bufnr)
    local qf_entries = {}

    if table.getn(picker:get_multi_selection()) > 0 then
        for _, entry in ipairs(picker:get_multi_selection()) do
            table.insert(qf_entries, entry_to_qf(entry))
        end
        vim.fn.setqflist(qf_entries, "r")
    else
        local manager = picker.manager
        for entry in manager:iter() do
            table.insert(qf_entries, entry_to_qf(entry))
        end

        vim.fn.setqflist(qf_entries, "r")
    end

    actions.select_default(prompt_bufnr)
end

require("telescope").load_extension("bibtex")
require("telescope").load_extension("gh")
require("telescope").load_extension("media_files")
require("telescope").load_extension("heading")
require("telescope").load_extension("refactoring")
-- require("telescope").load_extension("")

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
        -- layout_strategy = "vertical",
        -- layout_config = {
        --     vertical = {
        --         width = 100,
        --         preview_height = 40,
        --         height = 80,
        --     },
        -- },
        path_display = { shorten = 3 },
        pickers = {
            buffers = {
                show_all_buffers = true,
                sort_mru = true,
            },
            grep_string = {
                use_regex = true,
            },
        },
        mappings = {
            i = {
                -- ["<cr>"] = openAndList,
                ["<tab>"] = actions.move_selection_worse,
                ["<S-tab>"] = actions.move_selection_better,
                ["<c-u>"] = false,
                ["<c-a>"] = { "<home>", type = "command" },
                ["<c-e>"] = { "<end>", type = "command" },
                ["<C-q>"] = actions.smart_send_to_qflist,
                ["<C-Q>"] = actions.smart_add_to_qflist,
                ["<C-n>"] = actions.cycle_history_next,
                ["<C-p>"] = actions.cycle_history_prev,
                ["<C-space>"] = actions.toggle_selection + actions.move_selection_worse,
            },
            n = {
                -- ["<cr>"] = openAndList,
                ["<tab>"] = actions.move_selection_worse,
                ["<S-tab>"] = actions.move_selection_better,
                ["<C-q>"] = actions.smart_send_to_qflist,
                ["<C-Q>"] = actions.smart_add_to_qflist,
                ["<C-n>"] = actions.cycle_history_next,
                ["<C-p>"] = actions.cycle_history_prev,
                ["<C-space>"] = actions.toggle_selection + actions.move_selection_worse,
            },
        },
    },
    extensions = {
        bibtex = {
            depth = 2,
            global_files = { "/home/oleete/UniDrive/Thesis/thesis/Citations.bib" },
        },
        fzf = {
            override_generic_sorter = true,
            override_file_sorter = true,
            case_mode = "smart_case",
        },
        media_files = {},
    },
})

require("telescope").load_extension("file_browser")
require("telescope").load_extension("fzf")
require("telescope").load_extension("bibtex")

local open_dif = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    local cmd = "DiffviewOpen " .. value
    vim.api.nvim_set_var("DiffviewLast", cmd)
    vim.cmd(cmd)
end
local open_dif_mergebase = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    local cmd = "DiffviewOpen ..." .. value
    vim.api.nvim_set_var("DiffviewLast", cmd)
    vim.cmd(cmd)
end
local open_single_dif = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    local cmd = "DiffviewOpen " .. value .. "~1.." .. value
    vim.api.nvim_set_var("DiffviewLast", cmd)
    vim.cmd(cmd)
end
local change_gitsign_base = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    vim.api.nvim_win_close(0, true)
    local cmd = "Gitsigns change_base " .. value
    vim.cmd(cmd)
end

function _G.gitsign_change_base()
    require("telescope.builtin").git_commits({
        attach_mappings = function(_, map)
            map("n", "<cr>", change_gitsign_base)
            map("i", "<cr>", change_gitsign_base)
            return true
        end,
    })
end
function _G.gitsign_bchange_base()
    require("telescope.builtin").git_bcommits({
        attach_mappings = function(_, map)
            map("n", "<cr>", change_gitsign_base)
            map("i", "<cr>", change_gitsign_base)
            return true
        end,
    })
end

function _G.git_commits_againsthead()
    require("telescope.builtin").git_commits({
        attach_mappings = function(_, map)
            map("n", "<cr>", open_dif)
            map("i", "<cr>", open_dif)
            return true
        end,
    })
end
function _G.git_commits_onechange()
    require("telescope.builtin").git_commits({
        attach_mappings = function(_, map)
            map("n", "<cr>", open_single_dif)
            map("i", "<cr>", open_single_dif)
            return true
        end,
    })
end

function _G.git_branch_dif()
    require("telescope.builtin").git_branches({
        attach_mappings = function(_, map)
            map("n", "<cr>", open_dif)
            map("i", "<cr>", open_dif)
            return true
        end,
    })
end
function _G.git_branch_mergebase()
    require("telescope.builtin").git_branches({
        attach_mappings = function(_, map)
            map("n", "<cr>", open_dif_mergebase)
            map("i", "<cr>", open_dif_mergebase)
            return true
        end,
    })
end

-- function _G.project_files()
--     local opts = {}
--     local ok = pcall(require("telescope.builtin").git_files, opts)
--     if not ok then
--         require("telescope.builtin").find_files(opts)
--     end
-- end

function _G.project_files()
    local results = require("telescope.utils").get_os_command_output({ "git", "rev-parse", "--git-dir" })

    if results[1] then
        require("telescope.builtin").git_files(require("telescope.themes").get_ivy())
    else
        require("telescope.builtin").find_files(require("telescope.themes").get_ivy())
    end
end
