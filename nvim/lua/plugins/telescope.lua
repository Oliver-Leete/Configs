local tel_setup = function()
    local action_state = require("telescope.actions.state")
    local actions = require("telescope.actions")

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
            prompt_prefix = " ",
            selection_caret = " ",
            entry_prefix = "  ",
            multi_icon = "",
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
                    ["<c-a>"] = { "<home>", type = "command" },
                    ["<c-e>"] = { "<end>", type = "command" },
                    ["<c-u>"] = { "<c-u>", type = "command" },
                    ["<c-space>"] = actions.to_fuzzy_refine,
                    ["<C-q>"] = actions.smart_send_to_qflist,
                    ["<C-Q>"] = actions.smart_add_to_qflist,
                    ["<C-l>"] = actions.smart_send_to_qflist,
                    ["<c-s-l>"] = actions.smart_add_to_qflist,
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
                    ["<c-s-l>"] = actions.smart_add_to_qflist,
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

    require("telescope").load_extension("fzf")

    require('telescope-all-recent').setup({
        default = {
            disable = true,
            use_cwd = true,
            sorting = 'frecent'
        },
        pickers = {
            vim_ui_select = {
                kinds = {
                    codeaction = {
                        disable = true,
                    },
                    dap_run = {
                        use_cwd = true,
                        sorting = 'frecent',
                    },
                },
                prompts = {
                    ["Commander"] = {
                        disable = false,
                        use_cwd = false,
                        sorting = 'frecent',
                    },
                }
            },
        },
    })
end

return {
    "nvim-telescope/telescope.nvim",
    dependencies = {
        { "nvim-lua/plenary.nvim" },
        { "kyazdani42/nvim-web-devicons" },
        {
            "nvim-telescope/telescope-fzf-native.nvim",
            build = [[
                        cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release &&
                        cmake --build build --config Release &&
                        cmake --install build --prefix build
                    ]],
        },
        { "prochri/telescope-all-recent.nvim", dependencies = { "kkharji/sqlite.lua" } },
    },
    config = tel_setup,
}
