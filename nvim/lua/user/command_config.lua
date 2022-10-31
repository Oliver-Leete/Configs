-- Command Panel Bindings
local telescopeFileBrowser = "Telescope file_browser respect_gitignore=false theme=get_ivy"
GlobalCommands = {
    { source = "coverage", name = "Coverage summary", command = "CoverageSummary" },
    { source = "coverage", name = "Load coverage", command = "Coverage" },
    { source = "coverage", name = "Toggle coverage", command = "CoverageToggle" },

    { source = "default", name = "Message", command = "message" },
    { source = "default", name = "History", command = "Noice" },
    { source = "default", name = "Delete buffer", command = "bdelete" },
    { source = "default", name = "Clear search", command = "let @/=''" },
    { source = "default", name = "Close tab", command = "tabclose" },
    { source = "default", name = "Toggle text wraping", "set wrap!" },
    { source = "default", name = "File tree", command = "NvimTreeToggle" },
    { source = "default", name = "Undo tree", command = "UndotreeToggle" },
    { source = "default", name = "Reload snippets", command = "source ~/.config/nvim/after/plugin/luasnip.lua" },
    { source = "default", name = "Toggle Minimap", func = function() require("mini.map").toggle() end },

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

    { source = "git", name = "Diff against a commit", func = function() git_commits_againsthead() end, },
    { source = "git", name = "Diff of a branch from current", func = function() git_branch_dif() end, },
    { source = "git", name = "Diff of a branch from master", func = function() git_branch_mergebase() end, },
    { source = "git", name = "Diff of a commit", func = function() git_commits_onechange() end, },
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

Map("n", "<leader>p", function() CommandCentre({}, true) end)


local function append_command(runnables, to_add)
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

function CommandCentre(argCommands, extend)
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

Global_Runnables = {}


function Select_runnables()
    local runnables = {}

    -- Config sources
    local runnable_sources = { Global_Runnables, vim.g.runnables, vim.b[0].runnables }

    -- runnables from tasks.lua files in directory
    local task_files = vim.fn.systemlist([[fd -I tasks.lua]])
    if task_files then
        for name in pairs(task_files) do
            name = name:gsub("%./", ""):gsub("%.lua", "")
            table.insert(runnable_sources, require(name))
        end
    end

    for _, source in pairs(runnable_sources) do
        append_command(runnables, source)
    end

    if #runnables ~= 0 then
        CommandCentre(runnables)
    else
        vim.cmd.echomsg({ args = { "'Nothing to Run'" } })
    end
end
