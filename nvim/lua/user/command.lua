-- Command Panel Bindings
local telescopeFileBrowser = "Telescope file_browser respect_gitignore=false theme=get_ivy"
local genghis = require("genghis")
local tele = require("user.telescope")
local func = require("user.myfuncs")

GlobalCommands = {
    { source = "compiler", name = "Compiler explorer", command = "CECompile" },
    { source = "compiler", name = "Live compiler explorer", command = "CECompileLive" },
    { source = "compiler", name = "Hex toggle", func = require("hex").toggle },

    { source = "coverage", name = "Coverage summary", command = "CoverageSummary" },
    { source = "coverage", name = "Load coverage", command = "Coverage" },
    { source = "coverage", name = "Toggle coverage", command = "CoverageToggle" },

    { source = "default", name = "Message", command = "message" },
    { source = "default", name = "History", command = "Noice" },
    { source = "default", name = "Delete buffer", command = "Bdelete" },
    { source = "default", name = "Clear search", command = "let @/=''" },
    { source = "default", name = "Close tab", command = "tabclose" },
    { source = "default", name = "Toggle text wraping", "set wrap!" },
    { source = "default", name = "File tree", command = "Neotree" },
    { source = "default", name = "Undo list", command = "Telescope undo undo" },
    { source = "default", name = "Reload snippets", command = "source ~/.config/nvim/after/plugin/luasnip.lua" },
    { source = "default", name = "Toggle Minimap", func = require("mini.map").toggle },

    { source = "info", name = "Lazy", command = "Lazy" },
    { source = "info", name = "Lsp info", command = "LspInfo" },
    { source = "info", name = "Null-ls info", command = "NullLsInfo" },
    { source = "info", name = "Mason info", command = "Mason" },
    { source = "info", name = "Overseer info", command = "OverseerInfo" },

    { source = "file", name = "New file", func = genghis.createNewFile },
    { source = "file", name = "Rename file", func = genghis.renameFile },
    { source = "file", name = "Copy file", func = genghis.duplicateFile },
    { source = "file", name = "Make file executable", func = genghis.chmodx },
    { source = "file", name = "Trash file", func = func.trash_put },

    { source = "finders", name = "Buffers", command = "Telescope buffers theme=get_ivy" },
    { source = "finders", name = "Diagnostics", command = "Telescope diagnostics bufnr=0 theme=get_ivy" },
    { source = "finders", name = "File browser", command = telescopeFileBrowser },
    { source = "finders", name = "File browser (relative)", command = telescopeFileBrowser .. " cwd=%:p:h" },
    { source = "finders", name = "Files", command = "Telescope git_files theme=get_ivy" },
    { source = "finders", name = "Grep", command = "Telescope live_grep theme=get_ivy" },
    { source = "finders", name = "History (telescope)", command = "Telescope noice theme=get_ivy" },
    { source = "finders", name = "Old files finder", command = "Telescope oldfiles theme=get_ivy" },
    { source = "finders", name = "Quickfix", command = "Telescope quickfix theme=get_ivy" },
    { source = "finders", name = "Symbols", command = "Telescope lsp_document_symbols theme=get_ivy" },
    { source = "finders", name = "Todo list", command = "TodoTelescope theme=get_ivy" },
    { source = "finders", name = "Workspace diagnostics", command = "Telescope diagnostics theme=get_ivy" },
    { source = "finders", name = "Workspace symbols", command = "Telescope lsp_workspace_symbols theme=get_ivy" },

    { source = "settings", name = "Options", command = "Telescope vim_options theme=get_ivy" },
    { source = "settings", name = "Keymaps", command = "Telescope keymaps theme=get_ivy" },
    { source = "settings", name = "Highlights", command = "Telescope highlights theme=get_ivy" },
    { source = "settings", name = "Autocommands", command = "Telescope autocommands theme=get_ivy" },
    { source = "settings", name = "Help", command = "Telescope help_tags theme=get_ivy" },
    { source = "settings", name = "Man pages", command = "Telescope man_pages theme=get_ivy" },
    { source = "settings", name = "Reload Module", command = "Telescope reloader theme=get_ivy" },
    { source = "settings", name = "File types", command = "Telescope filetypes theme=get_ivy" },

    { source = "git", name = "Diff against a commit", func = tele.git_commits_againsthead, },
    { source = "git", name = "Diff of a branch from current", func = tele.git_branch_dif, },
    { source = "git", name = "Diff of a branch from master", func = tele.git_branch_mergebase, },
    { source = "git", name = "Diff of a commit", func = tele.git_commits_onechange, },
    { source = "git", name = "Diff of unstaged", command = "DiffviewOpen" },
    { source = "git", name = "File diff history", command = "DiffviewFileHistory %" },
    { source = "git", name = "Folder diff history", command = "DiffviewFileHistory" },
    { source = "git", name = "Reset File", command = "Gitsigns reset_buffer" },
    { source = "git", name = "Stage File", command = "Gitsigns stage_buffer" },

    { source = "profiling", name = "Profile Annotate Function", command = "PerfAnnotateFunction" },
    { source = "profiling", name = "Profile Cycle Format", command = "PerfCycleFormat" },
    { source = "profiling", name = "Profile Hottest Callers Function", command = "PerfHottestCallersFunction" },
    { source = "profiling", name = "Profile Hottest Callers Selection", command = "PerfHottestCallersSelection" },
    { source = "profiling", name = "Profile Hottest Lines", command = "PerfHottestLines" },
    { source = "profiling", name = "Profile Hottest Symbols", command = "PerfHottestSymbols" },
    { source = "profiling", name = "Profile Load Call Graph", command = "PerfLoadCallGraph" },
    { source = "profiling", name = "Profile Load Flame Graph", command = "PerfLoadFlameGraph" },
    { source = "profiling", name = "Profile Load Flat", command = "PerfLoadFlat" },
    { source = "profiling", name = "Profile Pick Event", command = "PerfPickEvent" },
    { source = "profiling", name = "Profile Toggle Annotations", command = "PerfToggleAnnotations" },

    { source = "tasks", name = "Run Tasks", command = "OverseerRun" },
    { source = "tasks", name = "Modify Tasks", command = "OverseerTaskAction" },
    { source = "tasks", name = "Task Window", command = "OverseerToggle" },
    { source = "tasks", name = "Test Window", func = function() require("neotest").summary.open() end },
    { source = "tasks", name = "Terminals", command = "Telescope termfinder theme=get_ivy" },
    { source = "tasks", name = "Clear Task Cache", command = "OverseerClearCache" },
}



local append_command = function(runnables, to_add)
    local function always_extend(dst, src)
        if not vim.tbl_islist(src) then
            src = vim.tbl_values(src)
        end
        vim.list_extend(dst, src)
    end

    if to_add then
        if type(to_add) == "table" then
            always_extend(runnables, to_add)
        elseif type(to_add) == "function" then
            always_extend(runnables, to_add())
        end
    end
end

local command_centre = function(argCommands, extend)
    if not extend then extend = false end
    if not argCommands then argCommands = {} end

    local commands = {}
    local command_sources = { argCommands }

    if extend then
        local default_sources = { GlobalCommands, vim.b[0].localCommands }
        for _, source in pairs(default_sources) do
            table.insert(command_sources, source)
        end
    end

    for _, source in pairs(command_sources) do
        append_command(commands, source)
    end

    table.sort(commands, function(a, b) return a.name < b.name end)
    table.sort(commands, function(a, b) return a.source < b.source end)

    vim.ui.select(commands, {
        prompt = "Command Centre",
        format_item = function(item)
            return "[" .. item.source:sub(1, 3) .. "] " .. item.name
        end,
        telescope = require("telescope.themes").get_ivy(),
    }, function(choice)
        if not choice then
            vim.cmd.echomsg({ args = { "'No command entered'" } })
            return
        end

        if choice.func ~= nil then
            choice.func()
        elseif choice.command ~= nil then
            vim.cmd(choice.command)
        elseif choice.keymap ~= nil then
            vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(choice.keymap, true, true, true), "n", false)
        else
            vim.cmd.echomsg({ args = { "'Command does not have an action'" } })
            return
        end
    end)
end

Map("n", "<leader>p", function() command_centre({}, true) end)
