require("gitsigns").setup({
    sign_priority = 6,
})
-- DiffView.nvim

local actions = require("diffview.config").actions
require("diffview").setup({
    diff_binaries = false,
    use_icons = true,
    key_bindings = {
        view = {
            ["<esc>"] = actions.focus_files,
        },
        file_panel = {
            ["<esc>"] = function() vim.cmd("DiffviewClose") end,
        },
        file_history_panel = {
            ["<esc>"] = function() vim.cmd("DiffviewClose") end,
        },
    },
})

-- conflict.nvim
require("git-conflict").setup({
    default_mappings = false,
    disable_diagnostics = false,
    highlights = {
        incoming = "DiffText",
        current = "DiffAdd",
    },
})

vim.api.nvim_create_autocmd("User", {
    pattern = "GitConflictDetected",
    callback = function()
        if vim.b[0].localCommands then
            table.insert(vim.b[0].localCommands, {
                source = "conflict",
                name = "List conflicts",
                command = "GitConflictListQf | Telescope quickfix theme=get_ivy",
            })
        else
            vim.b[0].localCommands = {
                {
                    source = "conflict",
                    name = "List conflicts",
                    command = "GitConflictListQf | Telescope quickfix theme=get_ivy",
                },
            }
        end
        Map("n", "co", "<Plug>(git-conflict-ours)")
        Map("n", "cb", "<Plug>(git-conflict-both)")
        Map("n", "c0", "<Plug>(git-conflict-none)")
        Map("n", "ct", "<Plug>(git-conflict-theirs)")
        Map("n", "[x", function() markGoCentre("GitConflictPrevConflict", "x") end)
        Map("n", "]x", function() markGoCentre("GitConflictNextConflict", "x") end)
    end,
})
