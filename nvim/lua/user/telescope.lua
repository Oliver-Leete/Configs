local action_state = require("telescope.actions.state")
local actions = require("telescope.actions")

require("telescope").load_extension("dap")
require("telescope").load_extension("refactoring")
require("telescope").load_extension("noice")
local command_center = require("command_center")

local open_dif = function()
    local selected_entry = action_state.get_selected_entry()
    S = selected_entry
    local value = selected_entry["value"]
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    vim.cmd("stopinsert")
    vim.schedule(function()
        vim.cmd("DiffviewOpen " .. value)
    end)
end
local open_dif_mergebase = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    vim.cmd("stopinsert")
    vim.schedule(function()
        vim.cmd("DiffviewOpen ..." .. value)
    end)
end
local open_single_dif = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry["value"]
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    vim.cmd("stopinsert")
    vim.schedule(function()
        vim.cmd("DiffviewOpen " .. value .. "~1.." .. value)
    end)
end

require("telescope").setup({
    defaults = {
        file_ignore_patterns = { "%.jld2" },
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
                ["<tab>"] = actions.toggle_selection + actions.move_selection_worse,
                ["<S-tab>"] = actions.toggle_selection + actions.move_selection_better,
                ["<c-u>"] = false,
                ["<c-a>"] = { "<home>", type = "command" },
                ["<c-e>"] = { "<end>", type = "command" },
                ["<c-space>"] = actions.to_fuzzy_refine,
                ["<C-q>"] = actions.smart_send_to_qflist,
                ["<C-Q>"] = actions.smart_add_to_qflist,
                ["<C-l>"] = actions.smart_send_to_qflist,
                ["<C-L>"] = actions.smart_add_to_qflist,
                ["<C-n>"] = actions.cycle_history_next,
                ["<C-p>"] = actions.cycle_history_prev,
                ["<C-j>"] = actions.preview_scrolling_down,
                ["<C-k>"] = actions.preview_scrolling_up,
                ["<C-cr>"] = actions.toggle_selection + actions.move_selection_worse,
            },
            n = {
                ["<tab>"] = actions.toggle_selection + actions.move_selection_worse,
                ["<S-tab>"] = actions.toggle_selection + actions.move_selection_better,
                ["<c-space>"] = actions.to_fuzzy_refine,
                ["<C-q>"] = actions.smart_send_to_qflist,
                ["<C-Q>"] = actions.smart_add_to_qflist,
                ["<C-l>"] = actions.smart_send_to_qflist,
                ["<C-L>"] = actions.smart_add_to_qflist,
                ["<C-n>"] = actions.cycle_history_next,
                ["<C-p>"] = actions.cycle_history_prev,
                ["<C-j>"] = actions.preview_scrolling_down,
                ["<C-k>"] = actions.preview_scrolling_up,
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
        undo = {
            side_by_side = true,
            layout_strategy = "vertical",
            diff_context_lines = 3,
            layout_config = {
                preview_height = 0.8,
            },
        },
    },
    pickers = {
        git_commits = {
            mappings = {
                i = {
                    ["<cr>"] = open_dif,
                    ["<s-cr>"] = open_single_dif,
                    ["<c-cr>"] = actions.select_default,
                },
                n = {
                    ["<cr>"] = open_dif,
                    ["<s-cr>"] = open_single_dif,
                    ["<c-cr>"] = actions.select_default,
                },
            },
        },
        git_stash = {
            mappings = {
                i = {
                    ["<cr>"] = open_dif,
                    ["<s-cr>"] = open_single_dif,
                    ["<c-cr>"] = actions.select_default,
                },
                n = {
                    ["<cr>"] = open_dif,
                    ["<s-cr>"] = open_single_dif,
                    ["<c-cr>"] = actions.select_default,
                },
            },
        },
        git_branch = {
            mappings = {
                i = {
                    ["<cr>"] = open_dif,
                    ["<s-cr>"] = open_dif_mergebase,
                    ["<c-cr>"] = actions.select_default,
                },
                n = {
                    ["<cr>"] = open_dif,
                    ["<s-cr>"] = open_dif_mergebase,
                    ["<c-cr>"] = actions.select_default,
                },
            },
        },
    },
})

require("telescope").load_extension("undo")
require("telescope").load_extension("command_center")
require("telescope").load_extension("fzf")

function ProjectFiles()
    local results = require("telescope.utils").get_os_command_output({ "git", "rev-parse", "--git-dir" })

    if results == nil then
        vim.notify("Something went wrong", vim.log.levels.WARN)
    elseif results[1] then
        require("telescope.builtin").git_files(vim.tbl_extend("force", require("telescope.themes").get_ivy(), {
        }))
    else
        require("telescope.builtin").find_files(vim.tbl_extend("force", require("telescope.themes").get_ivy(), {
        }))
    end
end

require('telescope-all-recent').setup({
    default = {
        disable = true,
        use_cwd = true,
        sorting = 'frecent'
    },
    pickers = {
        command_center = {
            disable = false,
            use_cwd = false,
            sorting = 'frecent',
        },
        ["dap#configurations"] = {
            disable = false,
            use_cwd = true,
            sorting = 'frecent',
        },
        vim_ui_select = {
            kinds = {
                resession_load = {
                    use_cwd = false,
                    sorting = 'frecent',
                },
                overseer_template = {
                    use_cwd = true,
                    sorting = 'frecent',
                },
                overseer_task_options = {
                    use_cwd = true,
                    sorting = 'frecent',
                },
                dap_run = {
                    use_cwd = true,
                    sorting = 'frecent',
                },
            }
        },
    },
})
