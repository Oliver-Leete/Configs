require("gitsigns").setup({
    sign_priority = 6,
    signs = {
        add = { hl = 'GitSignsAdd', text = '▌', numhl = 'GitSignsAddNr', linehl = 'GitSignsAddLn', culhl = 'GitSignsAddCul' },
        change = { hl = 'GitSignsChange', text = '▌', numhl = 'GitSignsChangeNr', linehl = 'GitSignsChangeLn', culhl = 'GitSignsChangeCul' },
        delete = { hl = 'GitSignsDelete', text = '▁', numhl = 'GitSignsDeleteNr', linehl = 'GitSignsDeleteLn', culhl = 'GitSignsDeleteCul' },
        topdelete = { hl = 'GitSignsDelete', text = '▔', numhl = 'GitSignsDeleteNr', linehl = 'GitSignsDeleteLn', culhl = 'GitSignsDeleteCul' },
        changedelete = { hl = 'GitSignsChange', text = '~', numhl = 'GitSignsChangeNr', linehl = 'GitSignsChangeLn', culhl = 'GitSignsChangeCul' },
    },
    culhl = true,
})

-- DiffView.nvim
local actions = require("diffview.config").actions
require("diffview").setup({
    diff_binaries = false,
    use_icons = true,
    hooks = {
        diff_buf_read = function(bufnr)
            vim.b[bufnr].is_diffview_file = true
        end
    },
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
