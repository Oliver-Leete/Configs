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
        ["<esc>"] = "<cmd>tabclose<cr>",
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
                ["iL"] = "@loop.inner",
            },
        },
        swap = {
            enable = true,
            swap_next = {
                ["g>"] = "@parameter.inner",
            },
            swap_previous = {
                ["g<"] = "@parameter.inner",
            },
        },
        move = {
            enable = false,
            -- goto_next_start = {
            --     ["]o"] = "@block.outer",
            --     ["]f"] = "@function.outer",
            -- },
            -- goto_next_end = {
            --     ["]O"] = "@block.outer",
            --     ["]F"] = "@function.outer"
            -- },
            -- goto_previous_start = {
            --     ["[o"] = "@block.outer",
            --     ["[f"] = "@function.outer",
            -- },
            -- goto_previous_end = {
            --     ["[O"] = "@block.outer",
            --     ["[F"] = "@function.outer",
            -- },
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

require('iswap').setup{
    keys = 'tnseriaodhgjplfuwy',
}
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
local saga = require("lspsaga")

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

vim.fn.sign_define( "LspDiagnosticsSignError", {texthl = "LspDiagnosticsSignError", text = "", numhl = "LspDiagnosticsSignError"})
vim.fn.sign_define( "LspDiagnosticsSignWarning", {texthl = "LspDiagnosticsSignWarning", text = "", numhl = "LspDiagnosticsSignWarning"})
vim.fn.sign_define( "LspDiagnosticsSignHint", {texthl = "LspDiagnosticsSignHint", text = "", numhl = "LspDiagnosticsSignHint"})
vim.fn.sign_define( "LspDiagnosticsSignInformation", {texthl = "LspDiagnosticsSignInformation", text = "", numhl = "LspDiagnosticsSignInformation"})

local custom_attach = function(client, bufnr)
	-- print("LSP started.");
    capabilities = capabilities
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
    saga.init_lsp_saga {
        use_saga_diagnostic_sign = true,
        error_sign = '',
        warn_sign = '',
        hint_sign = '',
        infor_sign = '',
        dianostic_header_icon = ' ',
        code_action_icon = '',
        code_action_prompt = {
            enable = true,
            sign = false,
            sign_priority = 20,
            virtual_text = true,
        },
        finder_definition_icon = '  ',
        finder_reference_icon = '  ',
        max_preview_lines = 10,
        finder_action_keys = { open = '<cr>', vsplit = 's',split = 'i',quit = '<esc>',scroll_down = '<C-f>', scroll_up = '<C-b>'},
        code_action_keys = { quit = '<esc>',exec = '<CR>' },
        rename_action_keys = { quit = '<esc>',exec = '<CR>' },
        definition_preview_icon = '  ',
        border_style = "single",
        rename_prompt_prefix = '➤',
    }
    require("which-key").register({
        ["<leader>"] = {
            ["."] = {"<cmd>Lspsaga code_action<CR>", "Code Actions"},
            o = {
                d = {"<cmd>Telescope lsp_definitions<cr>", "Definitions"},
                r = {"<cmd>Telescope lsp_references<cr>", "References"},
                i = {"<cmd>lua vim.lsp.buf.implementation()<CR>", "implementations"},
            },
            f = {
                s = {"<cmd>Telescope lsp_workspace_symbols<cr>", "Symbols"},
                S = {"<cmd>Telescope lsp_document_symbols<cr>", "Symbols (buffer)"},
            },
            p = {
                name = "preview",
                p = {"<Cmd>Lspsaga hover_doc<CR>", "Documentation"},
                s = {"<cmd>Lspsaga signature_help<CR>", "Signature"},
                d = {"<cmd>Lspsaga preview_definition<CR>", "Definition"},
                e = {"<cmd>Lspsaga show_line_diagnostics<CR>", "Diagnostics"},
                a = {"<cmd>Lspsaga lsp_finder<CR>", "All"},
                l = {"<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>", "Workspace Directory"},
            },
            r = {
                r = {"<cmd>Lspsaga rename<CR>", "Rename (LSP)"},
                ["="] = {"<cmd>lua vim.lsp.buf.formatting()<CR>", "Format"},
            },
        },
        ["["] = {
            e = {"<cmd>Lspsaga diagnostic_jump_prev<CR>", "Error"},
        },
        ["]"] = {
            e = {"<cmd>Lspsaga diagnostic_jump_next<CR>", "Error"},
        },
    }, {buffer=bufnr})
    require("which-key").register({
        ["<leader>"] = {
            ["."] = {"<cmd>Lspsaga range_code_action<CR>", "Code Actions"},
        }
    }, {mode="v", buffer=bufnr})
    vim.api.nvim_exec([[
        autocmd CursorHold * :Lspsaga show_cursor_diagnostics
    ]], false)
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
    root_dir = nvim_lsp.util.root_pattern('Project.toml', 'git', vim.fn.getcwd());
})

require("lspinstall").setup()
local servers = require("lspinstall").installed_servers()
for _, server in pairs(servers) do
    if server == 'texlab' then
        require("lspconfig").texlab.setup{
            on_attach=custom_attach,
            settings = {
                bibtex = {
                    formatting = {
                        lineLength = 120
                    }
                },
                latex = {
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
                    lint = {
                        onChange = true
                    }
                }
            }
         }
    elseif (server == 'lua') then
        require("lspconfig").lua.setup {
            on_attach=custom_attach,
            cmd = {
                "/home/oleete/.local/share/nvim/lspinstall/lua/sumneko-lua-language-server",
                "-E",
                "/home/oleete/.local/share/nvim/lspinstall/lua/sumneko-lua/extension/server/bin/linux/sumneko-lua-language-server"
            },
            root_dir = nvim_lsp.util.root_pattern("init.vim", "init.lua"),
            settings = {
                on_attach=custom_attach,
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

    else
        require("lspconfig")[server].setup{on_attach=custom_attach}
    end
end


require("lspconfig").hls.setup({
    on_attach=custom_attach,
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
        media_files = {
        },
    },
})

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

function _G.git_bcommits()
    require("telescope.builtin").git_bcommits({
        attach_mappings = function(_, map)
            map('n', '<c-o>', open_dif)
            map('i', '<c-o>', open_dif)
            map('n', '<c-d>', open_single_dif)
            map('i', '<c-d>', open_single_dif)
            return true
        end
    })
end

function _G.git_commits()
    require("telescope.builtin").git_commits({
        attach_mappings = function(_, map)
            map('n', '<c-o>', open_dif)
            map('i', '<c-o>', open_dif)
            map('n', '<c-d>', open_single_dif)
            map('i', '<c-d>', open_single_dif)
            return true
        end
    })
end

function _G.git_branch()
    require("telescope.builtin").git_branches({
        attach_mappings = function(_, map)
            map('n', '<c-o>', open_dif)
            map('i', '<c-o>', open_dif)
            map('n', '<c-d>', open_single_dif)
            map('i', '<c-d>', open_single_dif)
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

-- require("sniprun").setup({
--     selected_interpreters = {},
--     repl_enable = {},
--     repl_disable = {},
--     interpreter_options = {},
--     display = {
--         "VirtualTextOk",
--         "VirtualTextErr",
--         "LongTempFloatingWindow",
--         "Terminal"
--       },
--     snipruncolors = {
--         SniprunVirtualTextOk   =  {bg="#66eeff",fg="#000000",ctermbg="Cyan",cterfg="Black"},
--         SniprunFloatingWinOk   =  {fg="#66eeff",ctermfg="Cyan"},
--         SniprunVirtualTextErr  =  {bg="#881515",fg="#000000",ctermbg="DarkRed",cterfg="Black"},
--         SniprunFloatingWinErr  =  {fg="#881515",ctermfg="DarkRed"},
--     },
--     inline_messages = 0,
--     borders = 'single'
-- })
require("hop").setup({keys="tnseriaodhgjplfuwy"})

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
