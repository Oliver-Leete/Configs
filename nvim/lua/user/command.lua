-- Command Panel Bindings
local genghis = require("genghis")
local func = require("user.myfuncs")

local commander = require("commander")
commander.setup({
    components = {
        "CAT",
        "DESC",
        "KEYS",
    },
    sort_by = {
        "CAT",
        "DESC",
        "CMD",
        "KEYS",
    },
    integration = {
        telescope = {
            enable = true,
            -- theme = require("telescope.themes").get_dropdown
        },
    }
})

commander.add({
    { cat = "compiler", desc = "Compiler explorer",      cmd = "<cmd>CECompile<cr>" },
    { cat = "compiler", desc = "Live compiler explorer", cmd = "<cmd>CECompileLive<cr>" },
    { cat = "coverage", desc = "Coverage summary",       cmd = "<cmd>CoverageSummary<cr>" },
    { cat = "coverage", desc = "Load coverage",          cmd = "<cmd>Coverage<cr>" },
    { cat = "coverage", desc = "Toggle coverage",        cmd = "<cmd>CoverageToggle<cr>" },
    { cat = "default",  desc = "Message",                cmd = "<cmd>message<cr>" },
    { cat = "default",  desc = "History",                cmd = "<cmd>Noice<cr>" },
    { cat = "default",  desc = "Delete buffer",          cmd = require("mini.bufremove").delete },
    { cat = "default",  desc = "Clear search",           cmd = "<cmd>let @/=''<cr>" },
    {
        cat = "default",
        desc = "Rename Tab",
        cmd = function()
            vim.ui.input({ prompt = "Tab Name: " }, function(i) vim.t.tabname = i end)
        end
    },
    { cat = "default", desc = "Close tab",           cmd = "<cmd>tabclose<cr>" },
    { cat = "default", desc = "Toggle text wraping", cmd = "<cmd>set wrap!<cr>" },
    { cat = "default", desc = "Toggle autowrapping", cmd = func.toggle_autowrap },
    { cat = "default", desc = "Undo list",           cmd = "<cmd>Telescope undo undo<cr>" },
    {
        cat = "default",
        desc = "Edit Snippets",
        cmd = function()
            require("luasnip.loaders")
                .edit_snippet_files({ edit = function(file) vim.cmd("vsplit " .. file) end })
        end
    },
    {
        cat = "default",
        desc = "Delete Session",
        cmd = function()
            require("user.projects")
                .delete_session()
        end
    },
    {
        cat = "default",
        desc = "Clean Up Buffers",
        cmd = function()
            require('close_buffers')
                .delete({ type = 'hidden', force = true })
        end
    },
    { cat = "info", desc = "Lazy",                 cmd = "<cmd>Lazy<cr>" },
    { cat = "info", desc = "Mason",                cmd = "<cmd>Mason<cr>" },
    { cat = "info", desc = "Lsp info",             cmd = "<cmd>LspInfo<cr>" },
    { cat = "info", desc = "Null-ls info",         cmd = "<cmd>NullLsInfo<cr>" },
    { cat = "info", desc = "Overseer info",        cmd = "<cmd>OverseerInfo<cr>" },
    { cat = "file", desc = "File explorer",        cmd = function() require("mini.files").open() end },
    { cat = "file", desc = "New file",             cmd = genghis.createNewFile },
    { cat = "file", desc = "Rename file",          cmd = genghis.renameFile },
    { cat = "file", desc = "Copy file",            cmd = genghis.duplicateFile },
    { cat = "file", desc = "Make file executable", cmd = genghis.chmodx },
    { cat = "file", desc = "Trash file",           cmd = func.trash_put },
    {
        cat = "finders",
        desc = "Buffers",
        cmd =
        "<cmd>Telescope buffers theme=get_ivy<cr>"
    },
    {
        cat = "finders",
        desc = "Diagnostics",
        cmd =
        "<cmd>Telescope diagnostics bufnr=0 theme=get_ivy<cr>"
    },
    {
        cat = "finders",
        desc = "Files",
        cmd =
        "<cmd>Telescope git_files theme=get_ivy<cr>"
    },
    {
        cat = "finders",
        desc = "Grep",
        cmd =
        "<cmd>Telescope live_grep theme=get_ivy<cr>"
    },
    { cat = "finders", desc = "History (telescope)", cmd = "<cmd>Telescope noice theme=get_ivy<cr>" },
    {
        cat = "finders",
        desc = "Old files finder",
        cmd =
        "<cmd>Telescope oldfiles theme=get_ivy<cr>"
    },
    {
        cat = "finders",
        desc = "Quickfix",
        cmd =
        "<cmd>Telescope quickfix theme=get_ivy<cr>"
    },
    {
        cat = "finders",
        desc = "Symbols",
        cmd =
        "<cmd>Telescope lsp_document_symbols theme=get_ivy<cr>"
    },
    {
        cat = "finders",
        desc = "AST",
        cmd =
        "<cmd>Telescope ast_grep theme=get_ivy<cr>"
    },
    { cat = "finders", desc = "Todo list",           cmd = "<cmd>TodoTelescope theme=get_ivy<cr>" },
    {
        cat = "finders",
        desc = "Workspace diagnostics",
        cmd =
        "<cmd>Telescope diagnostics theme=get_ivy<cr>"
    },
    {
        cat = "finders",
        desc = "Workspace symbols",
        cmd =
        "<cmd>Telescope lsp_workspace_symbols theme=get_ivy<cr>"
    },
    {
        cat = "settings",
        desc = "Options",
        cmd =
        "<cmd>Telescope vim_options theme=get_ivy<cr>"
    },
    {
        cat = "settings",
        desc = "Keymaps",
        cmd =
        "<cmd>Telescope keymaps theme=get_ivy<cr>"
    },
    {
        cat = "settings",
        desc = "Highlights",
        cmd =
        "<cmd>Telescope highlights theme=get_ivy<cr>"
    },
    {
        cat = "settings",
        desc = "Autocommands",
        cmd =
        "<cmd>Telescope autocommands theme=get_ivy<cr>"
    },
    {
        cat = "settings",
        desc = "Help",
        cmd =
        "<cmd>Telescope help_tags theme=get_ivy<cr>"
    },
    {
        cat = "settings",
        desc = "Man pages",
        cmd =
        "<cmd>Telescope man_pages theme=get_ivy<cr>"
    },
    {
        cat = "settings",
        desc = "Reload Module",
        cmd =
        "<cmd>Telescope reloader theme=get_ivy<cr>"
    },
    {
        cat = "settings",
        desc = "File types",
        cmd =
        "<cmd>Telescope filetypes theme=get_ivy<cr>"
    },
    { cat = "git",       desc = "Commits",                           cmd = "<cmd>Telescope git_commits<cr>" },
    { cat = "git",       desc = "Branches",                          cmd = "<cmd>Telescope git_branches<cr>", },
    { cat = "git",       desc = "Stashes",                           cmd = "<cmd>Telescope git_stash<cr>", },
    { cat = "git",       desc = "Diff of unstaged",                  cmd = "<cmd>DiffviewOpen<cr>" },
    { cat = "git",       desc = "File diff history",                 cmd = "<cmd>DiffviewFileHistory %<cr>" },
    { cat = "git",       desc = "Folder diff history",               cmd = "<cmd>DiffviewFileHistory<cr>" },
    { cat = "git",       desc = "Reset File",                        cmd = "<cmd>Gitsigns reset_buffer<cr>" },
    { cat = "git",       desc = "Stage File",                        cmd = "<cmd>Gitsigns stage_buffer<cr>" },
    { cat = "dap",       desc = "Run Debugger",                      cmd = "<cmd>DapContinue<cr>" },
    { cat = "profiling", desc = "Profile Annotate Function",         cmd = "<cmd>PerfAnnotateFunction<cr>" },
    { cat = "profiling", desc = "Profile Cycle Format",              cmd = "<cmd>PerfCycleFormat<cr>" },
    { cat = "profiling", desc = "Profile Hottest Callers Function",  cmd = "<cmd>PerfHottestCallersFunction<cr>" },
    { cat = "profiling", desc = "Profile Hottest Callers Selection", cmd = "<cmd>PerfHottestCallersSelection<cr>" },
    { cat = "profiling", desc = "Profile Hottest Lines",             cmd = "<cmd>PerfHottestLines<cr>" },
    { cat = "profiling", desc = "Profile Hottest Symbols",           cmd = "<cmd>PerfHottestSymbols<cr>" },
    { cat = "profiling", desc = "Profile Load Call Graph",           cmd = "<cmd>PerfLoadCallGraph<cr>" },
    { cat = "profiling", desc = "Profile Load Flame Graph",          cmd = "<cmd>PerfLoadFlameGraph<cr>" },
    { cat = "profiling", desc = "Profile Load Flat",                 cmd = "<cmd>PerfLoadFlat<cr>" },
    { cat = "profiling", desc = "Profile Pick Event",                cmd = "<cmd>PerfPickEvent<cr>" },
    { cat = "profiling", desc = "Profile Toggle Annotations",        cmd = "<cmd>PerfToggleAnnotations<cr>" },
    { cat = "tasks",     desc = "Run Tasks",                         cmd = "<cmd>OverseerRun<cr>" },
    { cat = "tasks",     desc = "Modify Tasks",                      cmd = "<cmd>OverseerTaskAction<cr>" },
    { cat = "tasks",     desc = "Task Window",                       cmd = "<cmd>OverseerToggle<cr>" },
    {
        cat = "tasks",
        desc = "Test Window",
        cmd = function()
            require("neotest").summary
                .open()
        end
    },
    {
        cat = "tasks",
        desc = "Terminals",
        cmd =
        "<cmd>Telescope termfinder theme=get_ivy<cr>"
    },
    { cat = "tasks", desc = "Clear Task Cache", cmd = "<cmd>OverseerClearCache<cr>" },
    {
        cat = "lsp",
        desc = "Toggle code lens",
        cmd = function()
            vim.g.lsp_lens_on = not vim.g.lsp_lens_on
            if vim.g.lsp_lens_on then
                vim.lsp.codelens.refresh()
            else
                vim.lsp.codelens.clear()
            end
        end
    },
})

Map("n", "<leader>p", "<cmd>Telescope commander<cr>")
-- Map("n", "<leader>p", commander.show)
