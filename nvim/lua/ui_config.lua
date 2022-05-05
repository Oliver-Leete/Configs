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

-- Theme
vim.opt.termguicolors = true

require('kanagawa').setup({
    undercurl = true,
    commentStyle = "italic",
    functionStyle = "NONE",
    keywordStyle = "NONE",
    statementStyle = "NONE",
    typeStyle = "NONE",
    variablebuiltinStyle = "NONE",
    specialReturn = true,
    specialException = true,
    transparent = false,
    colors = {},
    overrides = {},
    dimInactive = true,
    globalStatus = true,
})

vim.cmd("colorscheme kanagawa")
-- Zen Mode

require("zen-mode").setup({
    window = {
        backdrop = 0.9, -- shade the backdrop of the Zen window. Set to 1 to keep the same as Normal
        width = 105,
        height = 1,
        options = {
            signcolumn = "no",
            number = false, -- disable number column
            relativenumber = false, -- disable relative numbers
            -- scrolloff = 999,
            wrap = true,
        },
    },
    plugins = {
        gitsigns = true, -- disables git signs
        options = {
            enabled = true,
            ruler = true,
            showcmd = true,
        }
    },
})

require('dressing').setup({
  select = {
    backend = { "telescope" },
    telescope = require('telescope.themes').get_ivy({
        height = 30,
    })
  },
})
