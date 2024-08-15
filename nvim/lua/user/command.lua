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
})

commander.add({
    { cat = "default",  desc = "Clear search",          cmd = "<cmd>let @/=''<cr>" },
    { cat = "tab",      desc = "Close tab",             cmd = "<cmd>tabclose<cr>" },
    { cat = "buffer",   desc = "Delete buffer",         cmd = require("mini.bufremove").delete },
    { cat = "buffers",  desc = "Buffers",               cmd = "<cmd>Telescope buffers theme=get_ivy<cr>" },
    { cat = "info",     desc = "Lazy",                  cmd = "<cmd>Lazy<cr>" },
    { cat = "info",     desc = "Mason",                 cmd = "<cmd>Mason<cr>" },
    { cat = "info",     desc = "Lsp info",              cmd = "<cmd>LspInfo<cr>" },
    { cat = "info",     desc = "None-ls info",          cmd = "<cmd>NullLsInfo<cr>" },
    { cat = "file",     desc = "New file",              cmd = genghis.createNewFile },
    { cat = "file",     desc = "Rename file",           cmd = genghis.renameFile },
    { cat = "file",     desc = "Move file",             cmd = genghis.moveAndRenameFile },
    { cat = "file",     desc = "Copy file",             cmd = genghis.duplicateFile },
    { cat = "file",     desc = "Move file to subdir",   cmd = genghis.moveToFolderInCwd },
    { cat = "file",     desc = "Trash file",            cmd = genghis.trashFile },
    { cat = "finders",  desc = "Files",                 cmd = "<cmd>Telescope git_files theme=get_ivy<cr>" },
    { cat = "finders",  desc = "Grep",                  cmd = "<cmd>Telescope live_grep theme=get_ivy<cr>" },
    { cat = "finders",  desc = "Quickfix",              cmd = "<cmd>Telescope quickfix theme=get_ivy<cr>" },
    { cat = "finders",  desc = "Symbols",               cmd = "<cmd>Telescope lsp_document_symbols theme=get_ivy<cr>" },
    { cat = "finders",  desc = "Workspace diagnostics", cmd = "<cmd>Telescope diagnostics theme=get_ivy<cr>" },
    { cat = "finders",  desc = "Workspace symbols",     cmd = "<cmd>Telescope lsp_dynamic_workspace_symbols theme=get_ivy<cr>" },
    { cat = "settings", desc = "Highlights",            cmd = "<cmd>Telescope highlights theme=get_ivy<cr>" },
    { cat = "diff",     desc = "Diff of unstaged",      cmd = "<cmd>DiffviewOpen<cr>" },
    { cat = "diff",     desc = "File diff history",     cmd = "<cmd>DiffviewFileHistory %<cr>" },
    { cat = "diff",     desc = "Folder diff history",   cmd = "<cmd>DiffviewFileHistory<cr>" },
    { cat = "dap",      desc = "Run Debugger",          cmd = "<cmd>DapContinue<cr>" },
    { cat = "settings", desc = "Toggle code lens",      cmd = func.codelens_toggle },
    {
        cat = "settings",
        desc = "Toggle inlay hints",
        cmd = function()
            vim.lsp.inlay_hint.enable(0,
                not vim.lsp.inlay_hint.is_enabled(0))
        end
    },
    { cat = "settings", desc = "Toggle text wraping", cmd = "<cmd>set wrap!<cr>" },
    { cat = "settings", desc = "Source file",         cmd = "<cmd>source %<cr>" },
    { cat = "settings", desc = "Toggle Diagnostics",  cmd = function() require("lsp_lines").toggle() end },
})
