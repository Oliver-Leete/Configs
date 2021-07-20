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

-- Auto Sessions

require("auto-session").setup({
    log_level = 'info',
    auto_session_enable_last_session = false,
    auto_session_enabled = true,
    auto_save_enabled = nil,
    auto_restore_enabled = nil,
    auto_session_suppress_dirs = nil
})

-- Autosave Setup

-- require("autosave").setup({
--     verbosity = 0,
--     enabled = true,
--     execution_message = "AutoSave: saved at " .. vim.fn.strftime("%H:%M:%S"),
--     events = {"InsertLeave", "TextChanged"},
--     conditions = {
--         exists = true,
--         filetype_is_not = {},
--         modifiable = true
--     },
--     write_all_buffers = true,
--     on_off_commands = true,
--     clean_command_line_interval = 2500
-- })

-- DiffView.nvim

local cb = require("diffview.config").diffview_callback
require("diffview").setup({
    diff_binaries = false,    -- Show diffs for binaries
    file_panel = {
        width = 35,
        use_icons = true        -- Requires nvim-web-devicons
    },
  key_bindings = {
    -- The `view` bindings are active in the diff buffers, only when the current
    -- tabpage is a Diffview.
    view = {
        ["<tab>"]     = cb("select_next_entry"),  -- Open the diff for the next file
        ["<s-tab>"]   = cb("select_prev_entry"),  -- Open the diff for the previous file
        ["<leader>x"] = cb("focus_files"),        -- Bring focus to the files panel
        ["<esc>"] = cb("focus_files")
        -- ["<leader>b"] = cb("toggle_files"),       -- Toggle the files panel.
    },
    file_panel = {
        ["j"]         = cb("next_entry"),         -- Bring the cursor to the next file entry
        ["<down>"]    = cb("next_entry"),
        ["k"]         = cb("prev_entry"),         -- Bring the cursor to the previous file entry.
        ["<up>"]      = cb("prev_entry"),
        ["<cr>"]      = cb("select_entry"),       -- Open the diff for the selected entry.
        ["o"]         = "<cmd>lua require('diffview').on_keypress('select_entry')<cr><cmd>sleep 100m<cr><cmd>DiffviewToggleFiles<cr>",
        ["p"]         = "<cmd>lua require('diffview').on_keypress('select_entry')<cr><cmd>DiffviewFocusFiles<cr>",
        ["R"]         = cb("refresh_files"),      -- Update stats and entries in the file list.
        ["<tab>"]     = cb("select_next_entry"),
        ["<s-tab>"]   = cb("select_prev_entry"),
        ["<leader>t"] = cb("focus_files"),
        ["<leader>b"] = cb("toggle_files"),
        ["<esc>"] = "<cmd>tabclose<cr>",
    }
  }
})


-- Treesitter

require("nvim-treesitter.configs").setup({
    autopairs = { enable = true },
    highlight = { enable = true },
    indent    = { enable = true },
    rainbow   = { enable = true },
    matchup   = {
        enable = true,
    },
    textobjects = {
        select = {
            enable = true,
            keymaps = {
                -- You can use the capture groups defined in textobjects.scm
                -- ["ao"] = "@class.outer",
                -- ["io"] = "@class.inner",
                ["af"] = "@function.outer",
                ["if"] = "@function.inner",
                ["aC"] = "@conditional.outer",
                ["iC"] = "@conditional.inner",
                ["ac"] = "@comment.outer",
                ["ic"] = "@comment.inner",
                ["aL"] = "@loop.outer",
                ["il"] = "@loop.inner",
                ["iB"] = "@block.inner",
                ["aB"] = "@block.outer",
            },
        },
        move = {
            enable = false,
            goto_next_start = {
                ["]o"] = "@class.outer",
                ["]f"] = "@function.outer",
            },
            goto_next_end = {
                ["]O"] = "@class.outer",
                ["]F"] = "@function.outer"
            },
            goto_previous_start = {
                ["[o"] = "@class.outer",
                ["[f"] = "@function.outer",
            },
            goto_previous_end = {
                ["[O"] = "@class.outer",
                ["[F"] = "@function.outer",
            },
        },
        lsp_interop = {
            enable = true,
            peek_definition_code = {
                ["<leader>pD"] = "@function.outer",
            },
         },
    },
    refactor = {
        highlight_current_scope = { enable = false },
        highlight_definitions = { enable = false },
        smart_rename = {
            enable = true,
            keymaps = {
                smart_rename = "<leader>rt",
            },
        },
        navigation = { enable = false },
    },
    playground = {
        enable = true,
        disable = {},
        updatetime = 25,
        persist_queries = false,
        keybindings = {
            toggle_query_editor       = 'o',
            toggle_hl_groups          = 'i',
            toggle_injected_languages = 't',
            toggle_anonymous_nodes    = 'a',
            toggle_language_display   = 'I',
            focus_language            = 'f',
            unfocus_language          = 'F',
            update                    = 'R',
            goto_node                 = '<cr>',
            show_help                 = '?',
        },
    },
    textsubjects = {
        enable = true,
        keymaps = {
            [";"] = 'textsubjects-smart',
        }
    },
})
require("nvim-biscuits").setup({
    on_events = { 'InsertLeave', 'CursorHoldI' },
    default_config = {
        min_distance = 10,
    },
    language_config = {
        haskell = {
            disabled = true
        }
    }
})
-- Compleation Setup

vim.o.completeopt = "menuone,noselect"
require("compe").setup({
    enabled = true;
    autocomplete = true;
    debug = false;
    min_length = 1;
    preselect = 'enable';
    throttle_time = 80;
    source_timeout = 200;
    incomplete_delay = 400;
    max_abbr_width = 100;
    max_kind_width = 100;
    max_menu_width = 100;
    documentation = {
        border = { '', '' ,'', ' ', '', '', '', ' ' },
        winhighlight = "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder",
        max_width = 120,
        min_width = 60,
        max_height = math.floor(vim.o.lines * 0.3),
        min_height = 1,
    };

    source = {
        path = true;
        buffer = true;
        calc = true;
        nvim_lsp = true;
        nvim_lua = true;
        nvim_treesitter = false;
        vsnip = true,
        omni = {
            filetypes = {'tex'},
        },
        tabnine = true
    };
})


-- AutoPairs Setup

require("nvim-autopairs").setup({
    check_ts = true,
    map_cr = true, --  map <CR> on insert mode
    map_complete = true, -- it will auto insert `(` after select function or method item
    enable_check_bracket_line = true,
})

-- Lsp Settup

local nvim_lsp = require("lspconfig")

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

local function preview_location_callback(_, _, result)
  if result == nil or vim.tbl_isempty(result) then
    return nil
  end
  vim.lsp.util.preview_location(result[1], {border="single"})
end

function PeekDefinition()
  local params = vim.lsp.util.make_position_params()
  return vim.lsp.buf_request(0, 'textDocument/definition', params, preview_location_callback)
end

local signs = { Error = " ", Warning = "", Hint = " ", Information = " " }
for type, icon in pairs(signs) do
  local hl = "LspDiagnosticsSign" .. type
  vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
end

-- Capture real implementation of function that sets signs
local orig_set_signs = vim.lsp.diagnostic.set_signs
local set_signs_limited = function(diagnostics, bufnr, client_id, sign_ns, opts)
    -- original func runs some checks, which I think is worth doing
    -- but maybe overkill
    if not diagnostics then
        diagnostics = diagnostic_cache[bufnr][client_id]
    end

    -- early escape
    if not diagnostics then
        return
    end

    -- Work out max severity diagnostic per line
    local max_severity_per_line = {}
    for _,d in pairs(diagnostics) do
        if max_severity_per_line[d.range.start.line] then
            local current_d = max_severity_per_line[d.range.start.line]
            if d.severity < current_d.severity then
                max_severity_per_line[d.range.start.line] = d
            end
        else
            max_severity_per_line[d.range.start.line] = d
        end
    end

    -- map to list
    local filtered_diagnostics = {}
    for _,v in pairs(max_severity_per_line) do
        table.insert(filtered_diagnostics, v)
    end

    -- call original function
    orig_set_signs(filtered_diagnostics, bufnr, client_id, sign_ns, opts)
end
vim.lsp.diagnostic.set_signs = set_signs_limited

local custom_attach = function(client, bufnr)
    print("LSP: " .. client.name .. " Started")
    capabilities = capabilities

    vim.lsp.handlers["textDocument/publishDiagnostics"] =
        function(_, _, params, client_id, _)
            local config = {
                underline = true,
                virtual_text = {
                    prefix = " ",
                    spacing = 4,
                },
                signs = true,
                update_in_insert = false,
            }
            local uri = params.uri
            local bufnr2 = vim.uri_to_bufnr(uri)

            if not bufnr2 then
                return
            end

            local diagnostics = params.diagnostics

            for i, v in ipairs(diagnostics) do
                diagnostics[i].message = string.format("%s: %s", v.source, v.message)
            end

            vim.lsp.diagnostic.save(diagnostics, bufnr2, client_id)

            if not vim.api.nvim_buf_is_loaded(bufnr2) then
                return
            end

            vim.lsp.diagnostic.display(diagnostics, bufnr2, client_id, config)
        end

    vim.lsp.handlers["textDocument/hover"] =
        vim.lsp.with(
        vim.lsp.handlers.hover,
        {
            border = "single"
        }
    )
    vim.lsp.handlers["textDocument/signatureHelp"] =
        vim.lsp.with(
        vim.lsp.handlers.signature_help,
        {
            border = "single"
        }
    )

    require("lsp_signature").on_attach({
        bind = true,
        doc_lines = 10,
        floating_window = true,
        fixpos = true,
        hint_enable = false,
        hint_prefix = "🐼 ",
        hint_scheme = "String",
        use_lspsaga = false,
        hi_parameter = "IncSearch",
        max_height = 12,
        max_width = 120,
        handler_opts = {
            border = "single"
        },
    })

    require("illuminate").on_attach(client)

    require("which-key").register({
        ["<leader>"] = {
            ["."] = {"<cmd>Telescope lsp_code_actions theme=get_cursor<CR>", "Code Actions"},
            o = {
                d = {"<cmd>Telescope lsp_definitions<cr>", "Definitions"},
                r = {"<cmd>Telescope lsp_references<cr>", "References"},
                i = {"<cmd>Telescope lsp_implementation<CR>", "implementations"},
            },
            f = {
                s = {"<cmd>Telescope lsp_workspace_symbols<cr>", "Symbols"},
                S = {"<cmd>Telescope lsp_document_symbols<cr>", "Symbols (buffer)"},
            },
            p = {
                name = "preview",
                p = {"<Cmd>lua vim.lsp.buf.hover({ focusable = false})<CR>", "Documentation"},
                s = {"<cmd>lua vim.lsp.buf.signature_help({ focusable = false})<CR>", "Signature"},
                d = {"<cmd>lua PeekDefinition()<CR>", "Definition"},
                e = {"<cmd>lua vim.lsp.diagnostic.show_line_diagnostics({ focusable = false, popup_opts = {border='single'}})<CR>", "Diagnostics"},
                l = {"<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", "Workspace Directory"},
            },
            r = {
                r = {"<cmd>lua vim.lsp.buf.rename()<CR>", "Rename (LSP)"},
                ["="] = {"<cmd>lua vim.lsp.buf.formatting()<CR>", "Format"},
            },
        },
        ["["] = {
            e = {"<cmd>lua vim.lsp.diagnostic.goto_prev({ focusable = false , popup_opts = { border = 'single' }})<CR>", "Error"},
        },
        ["]"] = {
            e = {"<cmd>lua vim.lsp.diagnostic.goto_next({ focusable = false , popup_opts = { border = 'single' }})<CR>", "Error"},
        },
    }, {buffer=bufnr})
    require("which-key").register({
        ["<leader>"] = {
            ["."] = {"<cmd>Telescope lsp_range_code_actions theme=get_cursor<CR>", "Code Actions"},
        }
    }, {mode="v", buffer=bufnr})

    vim.cmd [[autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb({sign={priority=7}})]]

    -- vim.api.nvim_exec([[
    --     autocmd CursorHold * :lua vim.lsp.diagnostic.show_line_diagnostics({ focusable = false , popup_opts = { border = 'single' }})
    -- ]], false)

    if client.resolved_capabilities.document_formatting then
        require("which-key").register({
            ["<leader>"] = {
            ["="] = {"<cmd>lua vim.lsp.buf.formatting()<CR>", "Format"},
            }
        })
        require("which-key").register({
            ["<leader>"] = {
            ["="] = {"<cmd>lua vim.lsp.buf.range_formatting()<CR>", "Format"},
            }
        }, {mode="v", buffer=bufnr})
    end

    if client.resolved_capabilities.document_highlight then
        vim.api.nvim_exec([[
            augroup lsp_document_highlight
                autocmd! * <buffer>
                autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
                autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
            augroup END
        ]], false)
    end
end
require("lspconfig").julials.setup({
    on_attach=custom_attach,
    flags = {debounce_text_changes=500},
})

require("lspinstall").setup()
local servers = require("lspinstall").installed_servers()
for _, server in pairs(servers) do
    if server == 'texlab' then
        require("lspconfig").texlab.setup{
            on_attach=custom_attach,
            flags = {debounce_text_changes=500},
            settings = {
                texlab = {
                    build = {
                        args = { "-pdf", "-interaction=nonstopmode", "-synctex=1", "%f" },
                        executable = "latexmk",
                        onSave = true,
                        forwardSearchAfter = true
                    },
                    forwardSearch = {
                        executable = "zathura",
                        args = {"--synctex-forward", "%l:1:%f", "%p"},
                        onSave = false
                    },
                    chktex = {
                        onEdit = true

                    }
                }
            }
        }
    elseif (server == 'lua') then
        require("lspconfig").lua.setup {
            on_attach=custom_attach,
            flags = {debounce_text_changes=500},
            cmd = {
                "/home/oleete/.local/share/nvim/lspinstall/lua/sumneko-lua-language-server",
                "-E",
                "/home/oleete/.local/share/nvim/lspinstall/lua/sumneko-lua/extension/server/bin/linux/sumneko-lua-language-server"
            },
            root_dir = nvim_lsp.util.root_pattern("init.vim", "init.lua"),
            settings = {
                Lua = {
                    runtime = {
                        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
                        version = 'LuaJIT',
                        -- Setup your lua path
                        path = vim.split(package.path, ';'),
                    },
                    diagnostics = {
                        -- Get the language server to recognize the `vim` global
                        globals = {'vim'},
                    },
                    workspace = {
                        -- Make the server aware of Neovim runtime files
                        library = {
                            [vim.fn.expand('$VIMRUNTIME/lua')] = true,
                            [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
                        },
                    },
                    -- Do not send telemetry data containing a randomized but unique identifier
                    telemetry = {
                        enable = false,
                    },
                },
            }
        }
    elseif server == 'diagnosticls' then
        -- require("lspconfig").diagnosticls.setup({
        --     on_attach=custom_attach,
        --     filetypes = {"tex"},
        --     initializationOptions = {},
        -- })
    else
        require("lspconfig")[server].setup{on_attach=custom_attach, flags={debounce_text_changes=500}}
    end
end


require("lspconfig").hls.setup({
    on_attach=custom_attach,
    flags = {debounce_text_changes=500},
    cmd = { "haskell-language-server-wrapper", "--lsp" },
    filetypes = { "haskell", "lhaskell" },
    root_dir = nvim_lsp.util.root_pattern("*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml", ".git"),
    lspinfo = function (cfg)
        -- return "specific"
        if cfg.settings.languageServerHaskell.logFile or false then
            return "logfile: "..cfg.settings.languageServerHaskell.logFile
        end
        return ""
    end;
})

require("lspkind").init({
    with_text = true,
    symbol_map = {
        Text = '',
        Method = 'ƒ',
        Function = '',
        Constructor = '',
        Variable = '',
        Class = '',
        Interface = 'ﰮ',
        Module = '',
        Property = '',
        Unit = '',
        Value = '',
        Enum = '了',
        Keyword = '',
        Snippet = '﬌',
        Color = '',
        File = '',
        Folder = '',
        EnumMember = '',
        Constant = '',
        Struct = ''
    },
})

vim.g.diagnostic_auto_popup_while_jump = 0
vim.g.diagnostic_enable_virtual_text = 0
vim.g.diagnostic_enable_underline = 0
vim.g.completion_timer_cycle = 200

-- Telescope Setup


local actions = require("telescope.actions")
local action_set = require("telescope.actions.set")
local extensions = require("telescope").extensions
require("telescope").load_extension("bibtex")
require("telescope").load_extension("gh")
require("telescope").load_extension("media_files")
require("telescope").load_extension("session-lens")
require("telescope").load_extension("heading")
local trouble = require("trouble.providers.telescope")

require("telescope").setup({
    defaults = {
        vimgrep_arguments = {
            'rg',
            '--color=never',
            '--no-heading',
            '--with-filename',
            '--line-number',
            '--column',
            '--smart-case'
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
        file_sorter =  require("telescope.sorters").get_fuzzy_file,
        file_ignore_patterns = {},
        generic_sorter =  require("telescope.sorters").get_generic_fuzzy_sorter,
        path_display = {"shorten"},
        winblend = 0,
        border = {},
        borderchars = {
            { '─', '│', '─', '│', '┌', '┐', '┘', '└'},
            results = {"─", "│", " ", "│", '┌', '┐', "│", "│"},
            prompt = {"─", "│", "─", "│", "├", "┤", "┘", "└"},
            preview = { '─', '│', '─', '│', '┌', '┐', '┘', '└'},
        },
        color_devicons = true,
        use_less = true,
        set_env = { ['COLORTERM'] = 'truecolor' }, -- default = nil,
        file_previewer = require("telescope.previewers").vim_buffer_cat.new,
        grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
        qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,

        mappings = {
            i = {
                ["<C-q>"] = actions.smart_send_to_qflist,
                ["<C-Q>"] = actions.smart_add_to_qflist,
                ["<C-l>"] = actions.smart_send_to_loclist,
                ["<C-L>"] = actions.smart_add_to_loclist,
                ["<c-e>"] = trouble.open_with_trouble,
                ["<c-E>"] = trouble.open_selected_with_trouble,
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
                ["<c-e>"] = trouble.open_with_trouble,
                ["<c-E>"] = trouble.open_selected_with_trouble,
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
            global_files = {'/home/oleete/UniDrive/1_Thesis/0.1_LaTeX/Citations.bib'},
        },
        fzf = {
            override_generic_sorter = false, -- override the generic sorter
            override_file_sorter = true,     -- override the file sorter
            case_mode = "smart_case",        -- or "ignore_case" or "respect_case"
        },
        hop = {
            keys = {"t", "n", "s", "e", "r", "i", "a", "o", "d", "h", "g", "j", "p", "l", "f", "u", "w", "y"},
            sign_hl = {"WarningMsg", "Title"},
            line_hl = {"CursorLine", "Noraml"},
            clear_selection_hl = true,
            trace_entry = true,
            reset_selection = true,
        },
        media_files = {
        },
    },
})

require('telescope').load_extension('hop')

local action_state = require("telescope.actions.state")

local open_dif = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry['value']
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    local cmd = 'DiffviewOpen ' .. value
    vim.cmd(cmd)
end
local open_single_dif = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry['value']
    -- close Telescope window properly prior to switching windows
    vim.api.nvim_win_close(0, true)
    local cmd = 'DiffviewOpen ' .. value .. '~1..' .. value
    vim.cmd(cmd)
end
local change_gitsign_base = function()
    local selected_entry = action_state.get_selected_entry()
    local value = selected_entry['value']
    vim.api.nvim_win_close(0,true)
    local cmd = 'Gitsigns change_base ' .. value
    vim.cmd(cmd)
end

function _G.gitsign_change_base()
    require("telescope.builtin").git_commits({
        attach_mappings = function(_, map)
            map('n', '<cr>', change_gitsign_base)
            map('i', '<cr>', change_gitsign_base)
            return true
        end
    })
end
function _G.gitsign_bchange_base()
    require("telescope.builtin").git_bcommits({
        attach_mappings = function(_, map)
            map('n', '<cr>', change_gitsign_base)
            map('i', '<cr>', change_gitsign_base)
            return true
        end
    })
end

function _G.git_commits_again()
    require("telescope.builtin").git_commits({
        attach_mappings = function(_, map)
            map('n', '<cr>', open_dif)
            map('i', '<cr>', open_dif)
            return true
        end
    })
end
function _G.git_commits_compe()
    require("telescope.builtin").git_commits({
        attach_mappings = function(_, map)
            map('n', '<cr>', open_single_dif)
            map('i', '<cr>', open_single_dif)
            return true
        end
    })
end

function _G.git_branch_again()
    require("telescope.builtin").git_branches({
        attach_mappings = function(_, map)
            map('n', '<cr>', open_dif)
            map('i', '<cr>', open_dif)
            return true
        end
    })
end
function _G.git_branch_comp()
    require("telescope.builtin").git_branches({
        attach_mappings = function(_, map)
            map('n', '<cr>', open_single_dif)
            map('i', '<cr>', open_single_dif)
            return true
        end
    })
end

require("telescope").load_extension('fzf')
require("telescope").load_extension('bibtex')


require("todo-comments").setup({
    signs = true,
    keywords = {
        FIX  = { icon = " ", color = "error", alt = { "FIXME", "BUG", "FIXIT", "FIX", "ISSUE" }, },
        TODO = { icon = " ", color = "info" },
        HACK = { icon = " ", color = "warning", alt = {"JANK", "WORKAROUND"}},
        WARN = { icon = " ", color = "warning", alt = { "WARNING"} },
        PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
        NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
    },
    highlight = {
        before  = "fg",
        keyword = "wide",
        after   = "fg",
    },
    colors = {
        error   = { "LspDiagnosticsDefaultError", "ErrorMsg", "#DC2626" },
        warning = { "LspDiagnosticsDefaultWarning", "WarningMsg", "#FBBF24" },
        info    = { "LspDiagnosticsDefaultInformation", "#2563EB" },
        hint    = { "LspDiagnosticsDefaultHint", "#10B981" },
        default = { "Identifier", "#7C3AED" },
    },
})

require"surround".setup{}

require("hop").setup({keys="tnseriaodhgjplfuwybkvmcxzq"})
require('iswap').setup{keys="tnseriaodhgjplfuwybkvmcxzq"}
require("tsht").config.hint_keys = {"t", "n", "s", "e", "r", "i", "a", "o", "d", "h", "g", "j", "p", "l", "f", "u", "w", "y"}
require("numb").setup()
require("foldsigns").setup()
require("range-highlight").setup()
require("colorizer").setup({'*'}, {
        RGB      = true;
        RRGGBB   = true;
        names    = false;
        RRGGBBAA = true;
        rgb_fn   = true;
        hsl_fn   = true;
        css_fn   = false;
        mode     = 'background';
})

local configs = require 'lspconfig/configs'
local util = require 'lspconfig/util'

local function readFiles(files)
    local dict = {}
    for _,file in pairs(files) do
        local f = io.open(file, "r")
        for l in f:lines() do
            table.insert(dict, l)
        end
    end
    return dict
end

local function findLtexLang()
    local buf_clients = vim.lsp.buf_get_clients()
    for _, client in pairs(buf_clients) do
        if client.name == "ltex" then
            return client.config.settings.ltex.language
        end
    end
end

local function findLtexFiles(filetype, value)
    local buf_clients = vim.lsp.buf_get_clients()
    for _, client in pairs(buf_clients) do
        if client.name == "ltex" then
            local files = nil
            if filetype == 'dictionary' then
                files = client.config.dictionary_files[value or findLtexLang()]
            elseif filetype == 'disable' then
                files = client.config.disabledrules_files[value or findLtexLang()]
            elseif filetype == 'falsePositive' then
                files = client.config.falsepositive_files[value or findLtexLang()]
            end

            if files then
                return files
            else
                return nil
            end
        end
    end
end

local function updateConfig(lang, configtype)
    local buf_clients = vim.lsp.buf_get_clients()
    local client = nil
    for _, lsp in pairs(buf_clients) do
        if lsp.name == "ltex" then
            client = lsp
        end
    end

    if client then
        if configtype == 'dictionary' then
            -- if client.config.settings.ltex.dictionary then
                client.config.settings.ltex.dictionary = {
                    [lang] = readFiles(client.config.dictionary_files[lang])
                };
                return client.notify('workspace/didChangeConfiguration', client.config.settings)
            -- else
                -- return vim.notify("Error when reading dictionary config, check it")
            -- end
        elseif configtype == 'disable' then
            if client.config.settings.ltex.disabledRules then
                client.config.settings.ltex.disabledRules = {
                    [lang] = readFiles(client.config.disabledrules_files[lang])
                };
                return client.notify('workspace/didChangeConfiguration', client.config.settings)
            else
                return vim.notify("Error when reading disabledRules config, check it")
            end

        elseif configtype == 'falsePositive' then
            if client.config.settings.ltex.hiddenFalsePositives then
                client.config.settings.ltex.hiddenFalsePositives = {
                    [lang] = readFiles(client.config.falsepositive_files[lang])
                };
                return client.notify('workspace/didChangeConfiguration', client.config.settings)
            else
                return vim.notify("Error when reading hiddenFalsePositives config, check it")
            end
        end
    else
        return nil
    end
end

local function addToFile(filetype, lang, file, value)
    file = io.open(file[#file-0], "a+") -- add only to last file defined.
    if file then
        file:write(value .. "\n")
        file:close()
    else
        return print("Failed insert %q", value)
    end
    if filetype == 'dictionary' then
        return updateConfig(lang, "dictionary")
    elseif filetype == 'disable' then
        return updateConfig(lang, "disable")
    elseif filetype == 'falsePositive' then
        return updateConfig(lang, "falsePositive")
    end
end

local function addTo(filetype, lang, file, value)
    local dict = readFiles(file)
    for _, v in ipairs(dict) do
        if v == value then
            return nil
        end
    end
    return addToFile(filetype, lang, file, value)
end

configs.ltex = {
    default_config = {
        cmd = {"/home/oleete/Downloads/ltex-ls-12.3.0/bin/ltex-ls"};
        filetypes = {'tex', 'bib', 'markdown'};
        dictionary_files = { ["en"] = {vim.fn.getcwd() .. "dictionary.ltex"} };
        disabledrules_files = { ["en"] = {vim.fn.getcwd() .. "disable.ltex"} };
        falsepositive_files = { ["en"] = {vim.fn.getcwd() .. "false.ltex"}};
        settings = {
            ltex = {
                enabled= {"latex", "tex", "bib", "markdown"},
                checkFrequency="save",
                language="en",
                diagnosticSeverity="information",
                setenceCacheSize=2000,
                additionalRules = {
                    enablePickyRules = true,
                    motherTongue= "en",
                };
                dictionary = {};
                disabledRules = {};
                hiddenFalsePositives = {};
            },
        };
        on_attach = function(client, bufnr)
                    -- local lang = client.config.settings.ltex.language
            for lang,_ in ipairs(client.config.dictionary_files) do       --
                    updateConfig(lang, "dictionary")
                    updateConfig(lang, "disable")
                    updateConfig(lang, "falsePositive")
            end
        end;
    };
};
--
-- https://github.com/neovim/nvim-lspconfig/issues/858 can't intercept,
-- override it then.
local orig_execute_command = vim.lsp.buf.execute_command
vim.lsp.buf.execute_command = function(command)
    if command.command == '_ltex.addToDictionary' then
        local arg = command.arguments[1].words -- can I really access like this?
        for lang, words in pairs(arg) do
            for _, word in ipairs(words) do
                local filetype = "dictionary"
                addTo(filetype,lang, findLtexFiles(filetype,lang), word)
            end
        end
    elseif command.command == '_ltex.disableRules' then
        local arg = command.arguments[1].ruleIds -- can I really access like this?
        for lang, rules in pairs(arg) do
            for _, rule in ipairs(rules) do
                local filetype = "disable"
                addTo(filetype,lang,findLtexFiles(filetype,lang), rule)
            end
        end

    elseif command.command == '_ltex.hideFalsePositives' then
        local arg = command.arguments[1].falsePositives -- can I really access like this?
        for lang, rules in pairs(arg) do
            for _, rule in ipairs(rules) do
                local filetype = "falsePositive"
                addTo(filetype,lang,findLtexFiles(filetype,lang), rule)
            end
        end
    else
        orig_execute_command(command)
    end
end
require("lspconfig").ltex.setup({
    on_attach = custom_attach,
    root_dir = nvim_lsp.util.root_pattern(".git"),
})
