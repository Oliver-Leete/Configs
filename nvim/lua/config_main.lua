-- Auto Sessions

require('auto-session').setup({
    log_level = 'info',
    auto_session_enable_last_session = false,
    auto_session_enabled = true,
    auto_save_enabled = nil,
    auto_restore_enabled = nil,
    auto_session_suppress_dirs = nil
})

-- Zen Mode

require("zen-mode").setup({
    window = {
        backdrop = 0.9, -- shade the backdrop of the Zen window. Set to 1 to keep the same as Normal
        width = 120,
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
    end,
    on_close = function()
    end,
})

-- BufferLine

require'bufferline'.setup({
    options = {
        view = "multiwindow",
        numbers = "none",
        number_style = "none",
        mappings = false,
        buffer_close_icon= '',
        modified_icon = '●',
        close_icon = '',
        left_trunc_marker = '',
        right_trunc_marker = '',
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
        separator_style = "slant",
        enforce_regular_tabs = true,
        always_show_bufferline = true,
    }
})

-- LuaLine Status Line

require('lualine').setup({
    options = {
        theme = 'tokyonight',
        section_separators = {'', ''},
        component_separators = {'', ''},
        extensions = { 'nvim-tree' },
    },
    sections = {
        lualine_a = {{'mode'}},
        lualine_b = {{'branch'}, { 'filename', file_status = true }},
        lualine_c = {
          { 'diagnostics',
            sources={ 'nvim_lsp' },
            sections={'error', 'warn', 'info'},
            color_error='#f85e84',
            color_warn='#e5c463',
            color_info='#7accd7',
            {error = '', warn = '', info = ''},
            'lsp-progress'
          },
        },
        lualine_x = {
            { 'diff',
                color_added='#9ecd6f',
                color_modified='#7accd7',
                color_removed='#f85e84',
                symbols={added = '+', modified = '~', removed = '-'}
            },
            'fileformat', 'filetype'
        },
        lualine_y = {'progress'},
        lualine_z = {'location'}
    },
    inactive_sections = {
        lualine_a = {},
        lualine_b = {},
        lualine_c = {},
        lualine_x = {},
        lualine_y = {},
        lualine_z = {}
    }
})

-- Nvim Tree
vim.g.nvim_tree_disable_netrw = 0
vim.g.nvim_tree_hijack_netrw = 0
vim.g.nvim_tree_git_hl = 1
vim.g.nvim_tree_lsp_diagnostics = 1
vim.g.netrw_liststyle = 3
vim.g.netrw_preview=1
vim.g.nvim_tree_width = 40

vim.g.nvim_tree_icons = {
    default = '',
    symlink = '',
    git = {unstaged = "", staged = "✓", unmerged = "", renamed = "➜", untracked = ""},
    folder = {default = "", open = "", empty = "", empty_open = "", symlink = ""}
}
local tree_cb = require'nvim-tree.config'.nvim_tree_callback
vim.g.nvim_tree_bindings = {
    ["<CR>"]           = tree_cb("edit"),
    ["o"]              = "<cmd>lua require('nvim-tree').on_keypress('edit')<cr><cmd>sleep 250m<cr><cmd>NvimTreeClose<cr>",
    ["<2-LeftMouse>"]  = tree_cb("edit"),
    ["<2-RightMouse>"] = tree_cb("cd"),
    ["<C-]>"]          = tree_cb("cd"),
    ["<C-v>"]          = tree_cb("vsplit"),
    ["<C-x>"]          = tree_cb("split"),
    ["<C-t>"]          = tree_cb("tabnew"),
    ["<"]              = tree_cb("prev_sibling"),
    [">"]              = tree_cb("next_sibling"),
    ["<BS>"]           = tree_cb("close_node"),
    ["<S-CR>"]         = tree_cb("close_node"),
    ["<Tab>"]          = tree_cb("preview"),
    ["I"]              = tree_cb("toggle_ignored"),
    ["H"]              = tree_cb("toggle_dotfiles"),
    ["R"]              = tree_cb("refresh"),
    ["a"]              = tree_cb("create"),
    ["dd"]             = tree_cb("remove"),
    ["r"]              = tree_cb("rename"),
    ["<C-r>"]          = tree_cb("full_rename"),
    ["x"]              = tree_cb("cut"),
    ["cc"]             = tree_cb("copy"),
    ["p"]              = tree_cb("paste"),
    ["[c"]             = tree_cb("prev_git_item"),
    ["]c"]             = tree_cb("next_git_item"),
    ["-"]              = tree_cb("dir_up"),
    ["q"]              = tree_cb("close"),
    ["<esc>"]          = tree_cb("close"),
}

-- DiffView.nvim

local cb = require'diffview.config'.diffview_callback
require'diffview'.setup({
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
        ["<leader>t"] = cb("focus_files"),        -- Bring focus to the files panel
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
    }
  }
})

-- Lsp Trouble

require("trouble").setup({
    height = 20,
    icons = true,
    mode = "workspace",
    fold_open = "",
    fold_closed = "",
    action_keys = {
        cancel = "q", -- cancel the preview and get back to your last window / buffer / cursor
        close = "<esc>", -- close the list
        refresh = "r", -- manually refresh
        jump = "<cr>", -- jump to the diagnostic or open / close folds
        jump_close = {"o"}, -- jump to the diagnostic and close the list
        toggle_mode = "m", -- toggle between "workspace" and "document" mode
        preview = "P", -- preview the diagnostic location
        toggle_preview = "p", -- preview the diagnostic location
        close_folds = {"zM", "zm"}, -- close all folds
        open_folds = {"zR", "zr"}, -- open all folds
        toggle_fold = {"zA", "za"}, -- toggle fold of current file
        previous = "k", -- preview item
        next = "j" -- next item
    },
    indent_lines = true, -- add an indent guide below the fold icons
    auto_open = false,
    auto_close = false,
    auto_preview = false, -- automatically preview the location of the diagnostic. <esc> to close preview and go back
    signs = {
        -- icons / text used for a diagnostic
        error       = "",
        warning     = "",
        hint        = "",
        information = "",
        other = "﫠",
    },
    use_lsp_diagnstic_signs = false -- enabling this will use the s
})

-- Symbols Outline

vim.g.symbols_outline = {
    highlight_hovered_item = true,
    show_guides = true,
    position = 'right',
    keymaps = {
        close = "<Esc>",
        goto_location = "<cr>",
        -- focus_location = "<cr>",
        hover_symbol = "<C-space>",
        rename_symbol = "r",
        code_actions = "a",
    },
    lsp_blacklist = {},
}

-- Toggle Terminal

require"toggleterm".setup({
    size            = 25,
    open_mapping    = [[<c-\>]],
    hide_numbers    = true,
    shade_filetypes = {},
    shade_terminals = false,
    shading_factor  = '1',
    start_in_insert = false,
    persist_size    = true,
    direction       = 'horizontal',
    shell           = 'fish',
})

-- Treesitter

require'nvim-treesitter.configs'.setup({
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
                ["ao"] = "@class.outer",
                ["io"] = "@class.inner",
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
    }
})
require('nvim-biscuits').setup({
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
require'compe'.setup({
    disabled = true;
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
    documentation = true;

    source = {
        path = true;
        buffer = true;
        calc = true;
        nvim_lsp = true;
        nvim_lua = true;
        nvim_treesitter = false;
        vsnip = {
            priority = 1000,
        },
        omni = {
            filetypes = 'tex'
        }
    };
})

-- Snippets Settup

local remap = vim.api.nvim_set_keymap
local npairs = require('nvim-autopairs')
_G.MUtils= {}

vim.g.completion_confirm_key = ""
MUtils.completion_confirm=function()
    if vim.fn.pumvisible() ~= 0  then
        if vim.fn.complete_info()["selected"] ~= -1 then
            return vim.fn["compe#confirm"](npairs.esc("<cr>"))
        else
            return npairs.esc("<cr>")
        end
    else
        return npairs.autopairs_cr()
    end
end

remap('i' , '<CR>','v:lua.MUtils.completion_confirm()', {expr = true , noremap = true})
npairs.setup({
    check_ts = true,
})

-- Lsp Settup

local nvim_lsp = require('lspconfig')
local saga = require('lspsaga')

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

vim.fn.sign_define( "LspDiagnosticsSignError", {texthl = "LspDiagnosticsSignError", text = "", numhl = "LspDiagnosticsSignError"})
vim.fn.sign_define( "LspDiagnosticsSignWarning", {texthl = "LspDiagnosticsSignWarning", text = "", numhl = "LspDiagnosticsSignWarning"})
vim.fn.sign_define( "LspDiagnosticsSignHint", {texthl = "LspDiagnosticsSignHint", text = "", numhl = "LspDiagnosticsSignHint"})
vim.fn.sign_define( "LspDiagnosticsSignInformation", {texthl = "LspDiagnosticsSignInformation", text = "", numhl = "LspDiagnosticsSignInformation"})

local custom_attach = function(client, bufnr)
	-- print("LSP started.");
    capabilities = capabilities
    require'lsp_signature'.on_attach()
    require 'illuminate'.on_attach(client)
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
                s = {"<cmd>Lspsaga signature_help<CR>", "Signiture"},
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
            ["="] = {"<cmd>lua vim.lsp.buf.formatting()<CR>", "Format"},
        }
    }, {mode="v", buffer=bufnr})
    vim.api.nvim_exec([[
        autocmd CursorHold * :Lspsaga show_cursor_diagnostics
    ]], false)
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
require'lspconfig'.julials.setup({
    on_attach=custom_attach,
    root_dir = nvim_lsp.util.root_pattern('Project.toml', 'git', vim.fn.getcwd());
})

require'lsp_signature'.on_attach({
    bind = true, -- This is mandatory, otherwise border config won't get registered.
    doc_lines = 10, -- only show one line of comment set to 0 if you do not want API comments be shown

    hint_enable = true, -- virtual hint enable
    hint_prefix = "🐼 ",  -- Panda for parameter
    hint_scheme = "String",

    handler_opts = {
        border = "single"   -- double, single, shadow, none
    },
    decorator = {"`", "`"}
})

require'lspinstall'.setup()
local servers = require'lspinstall'.installed_servers()
for _, server in pairs(servers) do
    if server == 'texlab' then
        require'lspconfig'.texlab.setup{
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
        require'lspconfig'.lua.setup {
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
        require'lspconfig'[server].setup{on_attach=custom_attach}
    end
end


require'lspconfig'.hls.setup({
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

require('lspkind').init({
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

-- Telescope Settup

local actions = require("telescope.actions")
require("telescope").load_extension("bibtex")
require("telescope").load_extension("gh")
require("telescope").load_extension("media_files")
require("telescope").load_extension("session-lens")
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
        prompt_position = "bottom",
        prompt_prefix = "> ",
        selection_caret = "> ",
        entry_prefix = "  ",
        initial_mode = "insert",
        selection_strategy = "reset",
        sorting_strategy = "descending",
        layout_strategy = "flex",
        layout_defaults = {
            horizontal = {
                mirror = false,
            },
            vertical = {
                mirror = false,
            },
        },
        file_sorter =  require'telescope.sorters'.get_fuzzy_file,
        file_ignore_patterns = {},
        generic_sorter =  require'telescope.sorters'.get_generic_fuzzy_sorter,
        shorten_path = true,
        winblend = 0,
        width = 0.75,
        preview_cutoff = 1,
        results_height = 1,
        results_width = 0.8,
        border = {},
        borderchars = { '─', '│', '─', '│', '╭', '╮', '╯', '╰' },
        color_devicons = true,
        use_less = true,
        set_env = { ['COLORTERM'] = 'truecolor' }, -- default = nil,
        file_previewer = require'telescope.previewers'.vim_buffer_cat.new,
        grep_previewer = require'telescope.previewers'.vim_buffer_vimgrep.new,
        qflist_previewer = require'telescope.previewers'.vim_buffer_qflist.new,

        mappings = {
            i = {
                ["<C-q>"] = actions.smart_send_to_qflist,
                ["<C-a>"] = actions.smart_add_to_qflist,
                ["<C-l>"] = actions.smart_send_to_loclist,
                ["<C-n>"] = actions.smart_add_to_loclist,
                ["<c-e>"] = trouble.open_with_trouble,
                ["<c-u>"] = trouble.open_selected_with_trouble,
                ["<C-space>"] = actions.toggle_selection + actions.move_selection_worse,
                ["<C-j>"] = actions.move_to_top,
                ["<C-h>"] = actions.move_to_middle,
                ["<C-k>"] = actions.move_to_bottom,
            },
            n = {
                ["<C-q>"] = actions.smart_send_to_qflist,
                ["<C-a>"] = actions.smart_add_to_qflist,
                ["<C-l>"] = actions.smart_send_to_loclist,
                ["<C-n>"] = actions.smart_add_to_loclist,
                ["<c-e>"] = trouble.open_with_trouble,
                ["<c-u>"] = trouble.open_selected_with_trouble,
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

local action_state = require('telescope.actions.state')

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
    require('telescope.builtin').git_bcommits({
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
    require('telescope.builtin').git_commits({
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
    require('telescope.builtin').git_branches({
        attach_mappings = function(_, map)
            map('n', '<c-o>', open_dif)
            map('i', '<c-o>', open_dif)
            map('n', '<c-d>', open_single_dif)
            map('i', '<c-d>', open_single_dif)
            return true
        end
    })
end

require('telescope').load_extension('fzf')
require('telescope').load_extension('bibtex')

-- Gitsigns Settup

require('gitsigns').setup({
    signs = {
        add          = {hl = 'GitSignsAdd',    text = '▌', numhl='GitSignsAddNr'   , linehl='GitSignsAddLn'},
        change       = {hl = 'GitSignsChange', text = '▌', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
        delete       = {hl = 'GitSignsDelete', text = '_', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
        topdelete    = {hl = 'GitSignsDelete', text = '‾', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
        changedelete = {hl = 'GitSignsDelete', text = '~', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
        base = nil,
        signcolumn = true,
        numhl = true,
        linehl = false,
    },
    signs_sec = {
        add          = {hl = 'Normal', text = '▌'},
        change       = {hl = 'Normal', text = '▌'},
        delete       = {hl = 'Normal', text = '_'},
        topdelete    = {hl = 'Normal', text = '‾'},
        changedelete = {hl = 'Normal', text = '~'},
        base = 'HEAD',
        signcolumn = true,
        numhl      = true,
        linehl     = false,
    },
    keymaps = {
        -- Default keymap options
        noremap = true,
        buffer = true,

        ['n ]h'] = { expr = true, "&diff ? ']c' : '<cmd>lua require\"gitsigns\".next_hunk()<CR>'"},
        ['n [h'] = { expr = true, "&diff ? '[c' : '<cmd>lua require\"gitsigns\".prev_hunk()<CR>'"},

        -- Text objects
        ['o ih'] = ':<C-U>lua require"gitsigns".select_hunk()<CR>',
        ['x ih'] = ':<C-U>lua require"gitsigns".select_hunk()<CR>'
    },
    watch_index = {
        interval = 1000
    },
    current_line_blame = false,
    sign_priority = 6,
    update_debounce = 100,
    status_formatter = nil, -- Use default
    use_decoration_api = true,
    use_internal_diff = true,  -- If luajit is present
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
        diffview = true
    },
    -- override/add mappings
    mappings = {
        -- modify status buffer mappings
        status = {
            ["B"] = "BranchPopup",
        }
    }
})

-- BQF Settup

require('bqf').setup({
    auto_enable = true,
    preview = {
        win_height = 12,
        win_vheight = 12,
        delay_syntax = 80,
        border_chars = {'│', '│', '─', '─', '╭', '╮', '╰', '╯', '█'}
    },
    func_map = {
        stoggledown = '<c-space>',
        stogglevm = '<c-space>',
        tab = '<c-t>',
        split = '<c-x>',
        prevfile = 'k',
        nextfile = 'j',
        vsplit = '<c-v>',
    },
})

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


require("which-key").register({
    g = {
        J = {"<cmd>SplitjoinJoin<cr>", "Smart Join"},
        K = {"<cmd>SplitjoinSplit<cr>", "Smart Split"},
        R = {"<plug>(SubversiveSubstituteToEndOfLine)", "Substitute to EOL"},
        r = {"<plug>(SubversiveSubstitute)", "Substitute"},
        rR = {"<plug>(SubversiveSubstitute)^", "Substitute to SOL"},
        rr = {"<plug>(SubversiveSubstituteLine)", "Substitute Line"},
        [":"] = {"Q", "Ex Mode"},
        ["<"] = {"Swap With Previous Argument"},
        [">"] = {"Swap With Next Argument"},
        O = {"O<Esc>", "Insert Blankline Before"},
        o = {"o<Esc>", "Insert Blankline"},
        j = {"J", "Join"},
        k = {"i<cr><esc>", "Split"},
        t = {"<Plug>(EasyAlign)", "Easy Allign"},
        i = {"<Plug>(ninja-insertstart)", "Insert in Object"},
        a = {"<Plug>(ninja-insertend)", "Append to Object"},
        c = {"Comment"},
        cc = {"Comment Line"},
        P = {
            name = "Paste Before",
            b = {"<Plug>UnconditionalPasteBlockBefore", "Paste Block"},
            B = {"<Plug>UnconditionalPasteJaggedBefore", "Paste Jagged"},
            I = {"<Plug>UnconditionalPasteCharBefore", "Paste Char"},
            i = {"<Plug>UnconditionalPasteInlinedBefore", "Paste Inline"},
            l = {"<plug>UnconditionalPasteLineBefore", "Paste Line"},
            S = {"<Plug>UnconditionalPasteParagraphedBefore", "Paste Paragraph"},
            s = {"<Plug>UnconditionalPasteSpacedBefore", "Paste Spaced"},
        },
        p = {
            name = "Paste After",
            b = {"<Plug>UnconditionalPasteBlockAfter", "Paste Block"},
            B = {"<Plug>UnconditionalPasteJaggedAfter", "Paste Jagged"},
            I = {"<Plug>UnconditionalPasteCharAfter", "Paste Char"},
            i = {"<Plug>UnconditionalPasteInlinedAfter", "Paste Inline"},
            l = {"<plug>UnconditionalPasteLineAfter", "Paste Line"},
            S = {"<Plug>UnconditionalPasteParagraphedAfter", "Paste Paragraph"},
            s = {"<Plug>UnconditionalPasteSpacedAfter", "Paste Spaced"},
        },
        s = {
            name = "Change Case",
            p = {"<Plug>CaserMixedCase", "Pascal Case"},
            c = {"<Plug>CaserCamelCase", "Camel Case"},
            ["_"] = {"<Plug>CaserSnakeCase", "Snake Case"},
            u = {"<Plug>CaserUpperCase", "Upper Case"},
            t = {"<Plug>CaserTitleCase", "Title Case"},
            d = {"<Plug>CaserSentenceCase", "Sentance Case"},
            ["<space>"] = {"<Plug>CaserSpaceCase", "Space Case"},
            ["-"] = {"<Plug>CaserKebabCase", "Kebab Case"},
            k = {"<Plug>CaserTitleKebabCase", "Title Kebab Case"},
            ["."] = {"<Plug>CaserDotCase", "Dot Case"},
            s ={
                name = "Change Case (Line)",
                p = {"gspix", "Pascal Case", noremap=false},
                c = {"gscix", "Camel Case", noremap=false},
                ["_"] = {"gs_ix", "Snake Case", noremap=false},
                u = {"gsuix", "Upper Case", noremap=false},
                t = {"gstix", "Title Case", noremap=false},
                d = {"gssix", "Sentance Case", noremap=false},
                ["<space>"] = {"gs<space>ix", "Space Case", noremap=false},
                ["-"] = {"gs-ix", "Kebab Case", noremap=false},
                k = {"gskix", "Title Kebab Case", noremap=false},
                ["."] = {"gs.ix", "Dot Case", noremap=false},
            },
            S = {
                name = "Change Case (SOL)",
                p = {"gspH", "Pascal Case", noremap=false},
                c = {"gscH", "Camel Case", noremap=false},
                ["_"] = {"gs_H", "Snake Case", noremap=false},
                u = {"gsuH", "Upper Case", noremap=false},
                t = {"gstH", "Title Case", noremap=false},
                d = {"gssH", "Sentance Case", noremap=false},
                ["<space>"] = {"gs<space>H", "Space Case", noremap=false},
                ["-"] = {"gs-H", "Kebab Case", noremap=false},
                k = {"gskH", "Title Kebab Case", noremap=false},
                ["."] = {"gs.H", "Dot Case", noremap=false},
            },
        },
        S = {
            name = "Change Case (EOL)",
            p = {"gspL", "Pascal Case", noremap=false},
            c = {"gscL", "Camel Case", noremap=false},
            ["_"] = {"gs_L", "Snake Case", noremap=false},
            u = {"gsuL", "Upper Case", noremap=false},
            t = {"gstL", "Title Case", noremap=false},
            d = {"gssL", "Sentance Case", noremap=false},
            ["<space>"] = {"gs<space>L", "Space Case", noremap=false},
            ["-"] = {"gs-L", "Kebab Case", noremap=false},
            k = {"gskL", "Title Kebab Case", noremap=false},
            ["."] = {"gs.L", "Dot Case", noremap=false},
        },
    },
    ["<leader>"] = {
        ["<leader>"] = {"<c-^>", "Alternate File"},
        ["/"] = {"<plug>(Telescope-relevant)", "Related Files"},
        [">"] = {"<cmd>Telescope spell_suggest<cr>", "Spelling Suggestions"},
        o = {
            name = "Open",
            f = {"gf", "Open File"},
        },
        F = {"<cmd>Telescope commands<cr>", "Commands"},
        f = {
            name = "Find",
            ["/"] = {"<cmd>Telescope search_history<cr>", "Search History"},
            [":"] = {"<cmd>Telescope command_history<cr>", "Search History"},
            B = {"<cmd>Telescope buffers only_cwd=true show_all_buffers=true<cr>", "Buffers (cwd)"},
            b = {"<cmd>Telescope buffers show_all_buffers=true<cr>", "Buffers"},
            C = {"<cmd>call v:lua.git_bcommits()<cr>", "Commits (buffer)"},
            c = {"<cmd>call v:lua.git_commits()<cr>", "Git Commits"},
            E = {"<cmd>Telescope lsp_document_diagnostics<cr>", "Errors (buffer)"},
            e = {"<cmd>Telescope lsp_workspace_diagnostics<cr>", "Errors"},
            F = {"<cmd>Telescope fd<cr>", "Files (non git)"},
            G = {"<cmd>Git! difftool<cr><cmd>cclose<cr><cmd>Telescope quickfix<cr>", "Git Chunks"},
            g = {"<cmd>Telescope git_status<cr>", "Git Status"},
            j = {"<cmd>Telescope jumplist<cr>", "Jumps"},
            l = {"<cmd>Telescope current_buffer_fuzzy_find<cr>", "Line"},
            m = {"<cmd>Telescope marks<cr>", "Marks"},
            n = {"<cmd>TodoTelescope<cr>", "Todo Items"},
            i = {"<cmd>Telescope media_files<cr>", "Images (and other media)"},
            o = {"<cmd>Telescope oldfiles<cr>", "Old Files"},
            Q = {"<cmd>Telescope loclist<cr>", "LocList"},
            q = {"<cmd>Telescope quickfix<cr>", "QuickFix"},
            r = {"<cmd>Telescope live_grep<cr>", "Grep"},
            R = {"<plug>(Telescope-grep)", "Fast Grep"},
            t = {"<cmd>Telescope treesitter<cr>", "Treesitter"},
            V = {"<plug>(Telescope-Vimgrep-files)", "Vim Grep (file select)"},
            v = {"<plug>(Telescope-Vimgrep-all)", "Vim Grep"},
            w = {"<plug>(Telescope-find)", "Find"},
            W = {"<plug>(Telescope-locate)", "Locate"},
            y = {"<cmd>Telescope registers<cr>", "Registers"},
            z = {"<cmd>Telescope session-lens search_session<cr>", "Session Search"},
        },
        G = {
            name = "GitHub",
            i = {"<cmd>Telescope gh issues<cr>", "Search Issues"},
            p = {"<cmd>Telescope gh pull_request<cr>", "Search Issues"},
            g = {"<cmd>Telescope gh gist<cr>", "Search Issues"},
            r = {"<cmd>Telescope gh run<cr>", "Search Issues"},
        },
        g = {
            name = "Git",
            a = {"<cmd>lua require'gitsigns'.blame_line()<CR>", "Blame Line"},
            A = {"<cmd>Gitsigns toggle_current_line_blame<CR>", "Blame Toggle"},
            b = {"<cmd>call v:lua.git_branch()<cr>", "Branches"},
            C = {"<cmd>call v:lua.git_bcommits()<cr>", "Commits (buffer)"},
            c = {"<cmd>call v:lua.git_commits()<cr>", "Commits"},
            d = {"<cmd>Gitsigns diffthis", "Diff View of Signs"},
            g = {"<cmd>call PMToggleView('gitdiff')<CR>", "Git Diff Viewer"},
            m = {"<cmd>Neogit commit<cr>", "Edit Commit Message"},
            n = {"<cmd>Neogit<cr>", "Neogit Status"},
            p = {"<cmd>lua require'gitsigns'.preview_hunk()<CR>", "Hunk Preview"},
            r = {"<cmd>lua require'gitsigns'.reset_hunk()<CR>", "Hunk Reset"},
            R = {"<cmd>Gitsigns reset_buffer<CR>", "Blame Toggle"},
            S = {"<cmd>Gitsigns stage_buffer<CR>", "Stage File"},
            s = {"<cmd>lua require'gitsigns'.stage_hunk()<CR>", "Hunk Stage"},
            u = {"<cmd>lua require'gitsigns'.undo_stage_hunk()<CR>", "Hunk Undo"},
            v = {"<cmd>Gitsigns select_hunk<CR>", "Blame Toggle"},
        },
        p = {
            name = "Preview",
            g = {"<cmd>lua require'gitsigns'.preview_hunk()<CR>", "Hunk Preview"},
            w = {"<cmd>MatchupWhereAmI??<cr>", "Preview Location"}
        },
        v = {
            name = "View",
            e = {"<cmd>call PMToggleView('errorlist')<CR>", "Error List"},
            E = {"<cmd>call PMToggleView('errorlistdoc')<CR>", "Error List (buffer)"},
            g = {"<cmd>call PMToggleView('gitdiff')<CR>", "Git"},
            i = {"<cmd>call PMToggleView('term')<CR>", "Terminal"},
            l = {"<cmd>call PMToggleView('loclist')<CR>", "Location List"},
            q = {"<cmd>call PMToggleView('quickfix')<CR>", "QuickFix List"},
            s = {"<cmd>call PMToggleView('symbols')<CR>", "Symbol List"},
            t = {"<cmd>call PMToggleView('nvim-tree')<CR>", "File Tree"},
            u = {"<cmd>call PMToggleView('undotree')<CR>", "Undo Tree"},
            v = {"<cmd>call CloseAllPanels()<cr>", "Close All Panels"},
        },
        Q = {"<cmd>CClear<cr><cmd>cgetbuffer<cr><cmd>TroubleRefresh<cr>", "Populater QF List With Buffer Errors "},
        q = {
            name = "QuickFix List",
            a = {"<cmd>caddbuffer<cr><cmd>TroubleRefresh<cr>", "Add Buffer Errrors to QF List"},
            c = {"<cmd>CClear<cr><cmd>TroubleRefresh<cr>", "Clear The List"},
            g = {"<cmd>Git! difftool<cr><cmd>Trouble quickfix<cr>", "Populate With Diffs"},
            n = {"<cmd>cnewer<cr>", "Newer List"},
            p = {"<cmd>colder<cr>", "Older List"},
            q = {"<cmd>call PMToggleView('quickfix')<CR>", "Open"},
            f = {"<cmd>call PMToggleView('quickfixFilter')<CR>", "Filter List"},
            V = {"<plug>(Quickfix-vimgrep-files)", "Populate With VimGrep (file select)"},
            v = {"<plug>(Quickfix-vimgrep-all)", "Populate With VimGrep"},
            w = {"<plug>(Quickfix-find)", "Populate With find"},
            W = {"<plug>(Quickfix-locate)", "Populate With Locate"},
            -- s = {"<cmd>SpellCheck!<cr>", "Populate With Spelling Errors"},
        },
        i = {
            name = "Interactive Terminal",
            i = {"<cmd>call PMToggleView('term')<CR>", "Open Terminal"},
        },
        L = {"<cmd>LClear<cr><cmd>lgetbuffer<cr><cmd>TroubleRefresh<cr>", "Populater LocList With Buffer Errors "},
        l = {
            name = "Location List",
            a = {"<cmd>laddbuffer<cr><cmd>TroubleRefresh<cr>", "Add Buffer Errrors to LocList"},
            c = {"<cmd>LClear<cr><cmd>TroubleRefresh<cr>", "Clear The List"},
            l = {"<cmd>call PMToggleView('loclist')<CR>", "Open Location List"},
            f = {"<cmd>call PMToggleView('loclistFilter')<CR>", "Filter List"},
            n = {"<cmd>lnewer<cr>", "Newer List"},
            p = {"<cmd>lolder<cr>", "Older List"},
            V = {"<plug>(Loclist-vimgrep-files)", "Populate With VimGrep (file select)"},
            v = {"<plug>(Loclist-vimgrep-all)", "Populate With VimGrep"},
            w = {"<plug>(Loclist-find)", "Populate With find"},
            W = {"<plug>(Loclist-locate)", "Populate With Locate"},
            -- s = {"<cmd>SpellLCheck!<cr>", "Populate With Spelling Errors"},
        },
        E = {"<cmd>CClear<cr><cmd>cgetbuffer<cr><cmd>TroubleRefresh<cr>", "Open Buffre Errors in Touble"},
        e = {
            name = "Errors",
            e = {"<cmd>call PMToggleView('errorlist')<CR>", "Open Errors"},
            E = {"<cmd>call PMToggleView('errorlistdoc')<CR>", "Open Errors (buffer)"},
            n = {"<cmd>call PMToggleView('Todo-Trouble')<cr>", "Todo Items"},
            f = {"<cmd>call PMToggleView('troubleTelescope')<CR>", "Open Telescope List"},
            r = {"<cmd>TroubleRefresh<cr>", "Refresh Errors"},
        },
        r = {
            name = "Refactor",
            s = {"<Plug>(Scalpel)", "Rename (Scalpel)"},
            t = {"Rename (Treesitter)"},
            v = {"<plug>(ExtractVar)", "Extract Variable"},
            d = {"[fyyO<esc><cmd>.!cat ~/.config/nvim/docstring/julia.txt<cr>pdw>>/TODO:<cr>", "Make Docstring", noremap=false}
        },
        t = {
            name = "Explorer",
            t = {"<cmd>call PMToggleView('nvim-tree')<cr>", "Open Explorer"},
        },
        b = {
            name = "buffers",
            o = {"<cmd>Bdelete hidden<cr>", "Close All Hidden Buffers"},
            d = {"<cmd>bdelete<cr>", "Delete the current buffer"},
        },
        w = {
            name = "Window Managment",
            w = {"<cmd>ZenMode<cr>", "Zen Mode"},
            o = {"<c-w>o", "Clean Up Windows"},
            ["<bs>"] = {"<c-w>c", "Close Window"},
            ["<cr>"] = {"<c-w>v", "Open Window"},
            x = {"<c-w>s", "Horizontal Split"},
            v = {"<c-w>v", "Vertical Split"},
            n = {"<C-W>w", "Next Window"},
            p = {"<C-W>W", "Previous Window"},
            N = {"<C-W>r", "Move Window Next"},
            P = {"<C-W>R", "Move Window Previous"},
            ["]"] = {"<cmd>vertical resize +5<cr>", "Vertical Resize"},
            ["["] = {"<cmd>vertical resize -5<cr>", "Vertical Resize"},
            ["}"] = {"<cmd>resize +5<cr>", "Horizontal Resize"},
            ["{"] = {"<cmd>resize -5<cr>", "Horizontal Resize"},
            ["="] = {"<c-w>=", "Equal Size"},
            h = {"<c-w>h", "Left Windown"},
            j = {"<c-w>j", "Below Window"},
            k = {"<c-w>k", "Above Window"},
            l = {"<c-w>l", "Right Window"},
            ["<left>"] = {"<c-w>h", "Left Windown"},
            ["<down>"] = {"<c-w>j", "Below Window"},
            ["<up>"] = {"<c-w>k", "Above Window"},
            ["<right>"] = {"<c-w>l", "Right Window"},
            H = {"<c-w>H", "Move Far Left"},
            J = {"<c-w>J", "Move Far Down"},
            K = {"<c-w>K", "Move Far Up"},
            L = {"<c-w>L", "Move Far Right"},
            c = {"<c-w>c", "Close Window"},
            ["/"] = {"<c-w>^", "Open Alternate File"},
            [","] = {"<cmd>BufferLineCyclePrev<cr>", "Previous Buffer"},
            ["."] = {"<cmd>BufferLineCycleNext<cr>", "Next Buffer"},
        },
        m = {
            name = "Make",
            m = {"<plug>(Julia-precompile)", "Precompile"},
            t = {"<plug>(Julia-test)", "Test Package"},
        },
        [","] = {
            name = "Settings",
            [","] = {"<cmd>Telescope vim_options<cr>", "Vim Options"},
            s = {"<cmd>set spell!<cr>", "Toggle Spelling"},
            k = {"<cmd>Telescope keymaps<cr>", "Keymaps"},
            c = {"<cmd>Telescope colorscheme<cr>", "Color Schemes"},
            C = {"<cmd>Telescope highlights<cr>", "Highlight Groups"},
            a = {"<cmd>Telescope autocommands<cr>", "AutoCommands"},
            f = {"<cmd>Telescope filetypes<cr>", "FileTypes"},
            h = {"<cmd>Telescope help_tags<cr>", "Help Tags"},
            m = {"<cmd>Telescope man_pages<cr>", "Man Pages"},
        },
    },
        ["1<leader>vi"] = { "<cmd>call PMToggleView('term')<CR>",  "Open Terminal 1"},
        ["2<leader>vi"] = { "<cmd>call PMToggleView('2term')<CR>", "Open Terminal 2"},
        ["3<leader>vi"] = { "<cmd>call PMToggleView('3term')<CR>", "Open Terminal 3"},
        ["4<leader>vi"] = { "<cmd>call PMToggleView('4term')<CR>", "Open Terminal 4"},
        ["5<leader>vi"] = { "<cmd>call PMToggleView('5term')<CR>", "Open Terminal 5"},
        ["1<leader>ii"] = { "<cmd>call PMToggleView('term')<CR>",  "Open Terminal 1"},
        ["2<leader>ii"] = { "<cmd>call PMToggleView('2term')<CR>", "Open Terminal 2"},
        ["3<leader>ii"] = { "<cmd>call PMToggleView('3term')<CR>", "Open Terminal 3"},
        ["4<leader>ii"] = { "<cmd>call PMToggleView('4term')<CR>", "Open Terminal 4"},
        ["5<leader>ii"] = { "<cmd>call PMToggleView('5term')<CR>", "Open Terminal 5"},
    ["["] = {
        name = "Backward Leader",
        L = {"<cmd>try <bar> lpfile <bar> catch /E553/ <bar> llast <bar> endtry<CR>", "Loclist File"},
        Q = {"<cmd>try <bar> cpfile <bar> catch /E553/ <bar> clast <bar> endtry<CR>", "QuickFix File"},
        l = {"<cmd>try <bar> lprevious <bar> catch /E553/ <bar> llast <bar> endtry<CR>", "LocList Entry"},
        q = {"<cmd>try <bar> cprevious <bar> catch /E553/ <bar> clast <bar> endtry<CR>", "QuickFix Entry"},
        t = {"<cmd>tabprevious<cr>", "Tab"},
        b = {"<cmd>BufferLineCyclePrev", "Buffer"},
        c = {"Hunk"},
        f = {"function"},
        F = {"function (end)"},
        o = {"Block"},
        O = {"Block (end)"},
        s = {"[s", "Spelling Mistake"},
        ["["] = {"Section"},
        ["]"] = {"Section (end)"},
    },
    ["]"] = {
        name = "Forward Leader",
        L = {"<cmd>try <bar> lnfile <bar> catch /E553/ <bar> lfirst <bar> endtry<CR>", "LocList File"},
        Q = {"<cmd>try <bar> cnfile <bar> catch /E553/ <bar> cfirst <bar> endtry<CR>", "QuickFix File"},
        l = {"<cmd>try <bar> lnext <bar> catch /E553/ <bar> lfirst <bar> endtry<CR>", "LocList Entry"},
        q = {"<cmd>try <bar> cnext <bar> catch /E553/ <bar> cfirst <bar> endtry<CR>", "QuickFix Entry"},
        t = {"<cmd>tabnext<cr>", "Tab"},
        b = {"<cmd>BufferLineCycleNext", "Buffer"},
        c = {"Hunk"},
        f = {"function"},
        F = {"function (end)"},
        o = {"Block"},
        O = {"Block (end)"},
        s = {"]s", "Spelling Mistake"},
        ["["] = {"Section (end)"},
        ["]"] = {"Section"},
    },
    ["<localleader>"] = {
        name = "Local Leader",
    },
})
require("which-key").register({
    ["<leader>"] = {
        r = {
            name = "Refactor",
            s = {"<Plug>(ScalpelVisual)", "Rename (Scalpel)"},
            v = {"<plug>(ExtractVarVisual)", "Extract Variable"}
        },
    },
    g = {
        R = {"<plug>(SubversiveSubstituteToEndOfLine)", "Substitute to EOL"},
        r = {"<plug>(SubversiveSubstitute)", "Substitute"},
        rr = {"<plug>(SubversiveSubstituteLine)", "Substitute Line"},
        rR = {"<plug>(SubversiveSubstitute)H", "Substitute to SOL"},
        j = {"J", "Join"},
        k = {"c<cr><esc>", "Split"},
        t = {"<Plug>(EasyAlign)", "Align"},
        s = {
            name = "Change Case",
            p = {"<Plug>CaserVMixedCase", "Pascal Case"},
            c = {"<Plug>CaserVCamelCase", "Camel Case"},
            ["_"] = {"<Plug>CaserVSnakeCase", "Snake Case"},
            u = {"<Plug>CaserVUpperCase", "Upper Case"},
            t = {"<Plug>CaserVTitleCase", "Title Case"},
            s = {"<Plug>CaserVSentenceCase", "Sentance Case"},
            ["<space>"] = {"<Plug>CaserVSpaceCase", "Space Case"},
            ["-"] = {"<Plug>CaserVKebabCase", "Kebab Case"},
            k = {"<Plug>CaserVTitleKebabCase", "Title Case"},
            ["."] = {"<Plug>CaserVDotCase", "Dot Case"},
        },
    },
    z = {
        i = {"I", "Insert"},
        a = {"A", "Append"},
    },
},{ mode = "v" })

require('numb').setup()
require('foldsigns').setup()
require("range-highlight").setup()
require('colorizer').setup({'*'}, {
        RGB      = true;         -- #RGB hex codes
        RRGGBB   = true;         -- #RRGGBB hex codes
        names    = false;         -- "Name" codes like Blue
        RRGGBBAA = true;        -- #RRGGBBAA hex codes
        rgb_fn   = true;        -- CSS rgb() and rgba() functions
        hsl_fn   = true;        -- CSS hsl() and hsla() functions
        css      = true;        -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
        css_fn   = true;        -- Enable all CSS *functions*: rgb_fn, hsl_fn
        mode     = 'background'; -- Set the display mode.
})
