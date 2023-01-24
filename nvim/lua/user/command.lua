-- Command Panel Bindings
local telescopeFileBrowser = "Telescope file_browser respect_gitignore=false theme=get_ivy"
local genghis = require("genghis")
local tele = require("user.telescope")
local func = require("user.myfuncs")

local command_center = require("command_center")

command_center.add({
    { category = "compiler", desc = "Compiler explorer", cmd = "<cmd>CECompile<cr>" },
    { category = "compiler", desc = "Live compiler explorer", cmd = "<cmd>CECompileLive<cr>" },
    { category = "compiler", desc = "Hex toggle", cmd = require("hex").toggle },

    { category = "coverage", desc = "Coverage summary", cmd = "<cmd>CoverageSummary<cr>" },
    { category = "coverage", desc = "Load coverage", cmd = "<cmd>Coverage<cr>" },
    { category = "coverage", desc = "Toggle coverage", cmd = "<cmd>CoverageToggle<cr>" },

    { category = "default", desc = "Message", cmd = "<cmd>message<cr>" },
    { category = "default", desc = "History", cmd = "<cmd>Noice<cr>" },
    { category = "default", desc = "Delete buffer", cmd = "<cmd>Bdelete<cr>" },
    { category = "default", desc = "Clear search", cmd = "<cmd>let @/=''<cr>" },
    { category = "default", desc = "Close tab", cmd = "<cmd>tabclose<cr>" },
    { category = "default", desc = "Toggle text wraping", cmd = "<cmd>set wrap!<cr>" },
    { category = "default", desc = "Undo list", cmd = "<cmd>Telescope undo undo<cr>" },
    { category = "default", desc = "Reload snippets", cmd = "<cmd>source ~/.config/nvim/after/plugin/luasnip.lua<cr>" },
    { category = "default", desc = "Toggle Minimap", cmd = require("mini.map").toggle },

    { category = "info", desc = "Lazy", cmd = "<cmd>Lazy<cr>" },
    { category = "info", desc = "Mason", cmd = "<cmd>Mason<cr>" },
    { category = "info", desc = "Lsp info", cmd = "<cmd>LspInfo<cr>" },
    { category = "info", desc = "Null-ls info", cmd = "<cmd>NullLsInfo<cr>" },
    { category = "info", desc = "Overseer info", cmd = "<cmd>OverseerInfo<cr>" },

    { category = "file", desc = "New file", cmd = genghis.createNewFile },
    { category = "file", desc = "Rename file", cmd = genghis.renameFile },
    { category = "file", desc = "Copy file", cmd = genghis.duplicateFile },
    { category = "file", desc = "Make file executable", cmd = genghis.chmodx },
    { category = "file", desc = "Trash file", cmd = func.trash_put },

    { category = "finders", desc = "Buffers", cmd = "<cmd>Telescope buffers theme=get_ivy<cr>" },
    { category = "finders", desc = "Diagnostics", cmd = "<cmd>Telescope diagnostics bufnr=0 theme=get_ivy<cr>" },
    { category = "finders", desc = "File browser", cmd = telescopeFileBrowser },
    { category = "finders", desc = "File browser (relative)", cmd = telescopeFileBrowser .. " cwd=%:p:h" },
    { category = "finders", desc = "Files", cmd = "<cmd>Telescope git_files theme=get_ivy<cr>" },
    { category = "finders", desc = "Grep", cmd = "<cmd>Telescope live_grep theme=get_ivy<cr>" },
    { category = "finders", desc = "History (telescope)", cmd = "<cmd>Telescope noice theme=get_ivy<cr>" },
    { category = "finders", desc = "Old files finder", cmd = "<cmd>Telescope oldfiles theme=get_ivy<cr>" },
    { category = "finders", desc = "Quickfix", cmd = "<cmd>Telescope quickfix theme=get_ivy<cr>" },
    { category = "finders", desc = "Symbols", cmd = "<cmd>Telescope lsp_document_symbols theme=get_ivy<cr>" },
    { category = "finders", desc = "Todo list", cmd = "<cmd>TodoTelescope theme=get_ivy<cr>" },
    { category = "finders", desc = "Workspace diagnostics", cmd = "<cmd>Telescope diagnostics theme=get_ivy<cr>" },
    { category = "finders", desc = "Workspace symbols", cmd = "<cmd>Telescope lsp_workspace_symbols theme=get_ivy<cr>" },

    { category = "settings", desc = "Options", cmd = "<cmd>Telescope vim_options theme=get_ivy<cr>" },
    { category = "settings", desc = "Keymaps", cmd = "<cmd>Telescope keymaps theme=get_ivy<cr>" },
    { category = "settings", desc = "Highlights", cmd = "<cmd>Telescope highlights theme=get_ivy<cr>" },
    { category = "settings", desc = "Autocommands", cmd = "<cmd>Telescope autocommands theme=get_ivy<cr>" },
    { category = "settings", desc = "Help", cmd = "<cmd>Telescope help_tags theme=get_ivy<cr>" },
    { category = "settings", desc = "Man pages", cmd = "<cmd>Telescope man_pages theme=get_ivy<cr>" },
    { category = "settings", desc = "Reload Module", cmd = "<cmd>Telescope reloader theme=get_ivy<cr>" },
    { category = "settings", desc = "File types", cmd = "<cmd>Telescope filetypes theme=get_ivy<cr>" },

    { category = "git", desc = "Diff against a commit", cmd = tele.git_commits_againsthead, },
    { category = "git", desc = "Diff of a branch from current", cmd = tele.git_branch_dif, },
    { category = "git", desc = "Diff of a branch from master", cmd = tele.git_branch_mergebase, },
    { category = "git", desc = "Diff of a commit", cmd = tele.git_commits_onechange, },
    { category = "git", desc = "Diff of unstaged", cmd = "<cmd>DiffviewOpen<cr>" },
    { category = "git", desc = "File diff history", cmd = "<cmd>DiffviewFileHistory %<cr>" },
    { category = "git", desc = "Folder diff history", cmd = "<cmd>DiffviewFileHistory<cr>" },
    { category = "git", desc = "Reset File", cmd = "<cmd>Gitsigns reset_buffer<cr>" },
    { category = "git", desc = "Stage File", cmd = "<cmd>Gitsigns stage_buffer<cr>" },

    { category = "profiling", desc = "Profile Annotate Function", cmd = "<cmd>PerfAnnotateFunction<cr>" },
    { category = "profiling", desc = "Profile Cycle Format", cmd = "<cmd>PerfCycleFormat<cr>" },
    { category = "profiling", desc = "Profile Hottest Callers Function", cmd = "<cmd>PerfHottestCallersFunction<cr>" },
    { category = "profiling", desc = "Profile Hottest Callers Selection", cmd = "<cmd>PerfHottestCallersSelection<cr>" },
    { category = "profiling", desc = "Profile Hottest Lines", cmd = "<cmd>PerfHottestLines<cr>" },
    { category = "profiling", desc = "Profile Hottest Symbols", cmd = "<cmd>PerfHottestSymbols<cr>" },
    { category = "profiling", desc = "Profile Load Call Graph", cmd = "<cmd>PerfLoadCallGraph<cr>" },
    { category = "profiling", desc = "Profile Load Flame Graph", cmd = "<cmd>PerfLoadFlameGraph<cr>" },
    { category = "profiling", desc = "Profile Load Flat", cmd = "<cmd>PerfLoadFlat<cr>" },
    { category = "profiling", desc = "Profile Pick Event", cmd = "<cmd>PerfPickEvent<cr>" },
    { category = "profiling", desc = "Profile Toggle Annotations", cmd = "<cmd>PerfToggleAnnotations<cr>" },

    { category = "tasks", desc = "Run Tasks", cmd = "<cmd>OverseerRun<cr>" },
    { category = "tasks", desc = "Modify Tasks", cmd = "<cmd>OverseerTaskAction<cr>" },
    { category = "tasks", desc = "Task Window", cmd = "<cmd>OverseerToggle<cr>" },
    { category = "tasks", desc = "Test Window", cmd = function() require("neotest").summary.open() end },
    { category = "tasks", desc = "Terminals", cmd = "<cmd>Telescope termfinder theme=get_ivy<cr>" },
    { category = "tasks", desc = "Clear Task Cache", cmd = "<cmd>OverseerClearCache<cr>" },
})



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

Map("n", "<leader>p", "<cmd>Telescope command_center<cr>")
