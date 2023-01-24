local M = {}

local action_state = require("telescope.actions.state")
local actions = require("telescope.actions")

require("telescope").load_extension("dap")
require("telescope").load_extension("refactoring")
require("telescope").load_extension("noice")
require("telescope").load_extension("undo")
local command_center = require("command_center")

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
            "--multiline",
            "--vimgrep",
        },
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
                ["<tab>"] = actions.move_selection_worse,
                ["<S-tab>"] = actions.move_selection_better,
                ["<c-u>"] = false,
                ["<c-a>"] = { "<home>", type = "command" },
                ["<c-e>"] = { "<end>", type = "command" },
                ["<c-f>"] = actions.to_fuzzy_refine,
                ["<C-q>"] = actions.smart_send_to_qflist,
                ["<C-Q>"] = actions.smart_add_to_qflist,
                ["<C-n>"] = actions.cycle_history_next,
                ["<C-p>"] = actions.cycle_history_prev,
                ["<C-cr>"] = actions.toggle_selection + actions.move_selection_worse,
            },
            n = {
                ["<tab>"] = actions.move_selection_worse,
                ["<S-tab>"] = actions.move_selection_better,
                ["<c-f>"] = actions.to_fuzzy_refine,
                ["<C-q>"] = actions.smart_send_to_qflist,
                ["<C-Q>"] = actions.smart_add_to_qflist,
                ["<C-n>"] = actions.cycle_history_next,
                ["<C-p>"] = actions.cycle_history_prev,
                ["<C-cr>"] = actions.toggle_selection + actions.move_selection_worse,
            },
        },
    },
    extensions = {
        fzf = {
            override_generic_sorter = true,
            override_file_sorter = true,
            case_mode = "smart_case",
        },
        lsp_handlers = {
            require("telescope.themes").get_dropdown(),
        },
        command_center = {
            components = {
                command_center.component.CATEGORY,
                command_center.component.DESC,
                command_center.component.KEYS,
            },
            sort_by = {
                command_center.component.CATEGORY,
                command_center.component.DESC,
            },
            theme = require("telescope.themes").get_dropdown,
        },
    },
})

require("telescope").load_extension("command_center")
require("telescope").load_extension("file_browser")
require("telescope").load_extension("fzf")

local open_dif = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    local cmd = "DiffviewOpen " .. value
    vim.cmd(cmd)
end
local open_dif_mergebase = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    local cmd = "DiffviewOpen ..." .. value
    vim.cmd(cmd)
end
local open_single_dif = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    local cmd = "DiffviewOpen " .. value .. "~1.." .. value
    vim.cmd(cmd)
end

function M.git_commits_againsthead()
    require("telescope.builtin").git_commits({
        attach_mappings = function(_, map)
            map("n", "<cr>", open_dif)
            map("i", "<cr>", open_dif)
            return true
        end,
    })
end

function M.git_commits_onechange()
    require("telescope.builtin").git_commits({
        attach_mappings = function(_, map)
            map("n", "<cr>", open_single_dif)
            map("i", "<cr>", open_single_dif)
            return true
        end,
    })
end

function M.git_branch_dif()
    require("telescope.builtin").git_branches({
        attach_mappings = function(_, map)
            map("n", "<cr>", open_dif)
            map("i", "<cr>", open_dif)
            return true
        end,
    })
end

function M.git_branch_mergebase()
    require("telescope.builtin").git_branches({
        attach_mappings = function(_, map)
            map("n", "<cr>", open_dif_mergebase)
            map("i", "<cr>", open_dif_mergebase)
            return true
        end,
    })
end

function ProjectFiles()
    local results = require("telescope.utils").get_os_command_output({ "git", "rev-parse", "--git-dir" })

    if results == nil then
        vim.cmd.echomsg({ args = { "'Something went wrong'" } })
    elseif results[1] then
        require("telescope.builtin").git_files(require("telescope.themes").get_ivy())
    else
        require("telescope.builtin").find_files(require("telescope.themes").get_ivy())
    end
end

return M
