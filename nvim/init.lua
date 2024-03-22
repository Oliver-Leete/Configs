vim.api.nvim_set_var("$SHELL", "/bin/zsh")
vim.opt.shell = "/bin/zsh"

Map = vim.keymap.set
vim.api.nvim_set_var("mapleader", " ")
vim.api.nvim_set_var("maplocalleader", "\\")

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable",
        lazypath,
    })
end
vim.opt.rtp:prepend(lazypath)

require("user.settings")

require("lazy").setup(
    {
        -- Misc
        { "pocco81/auto-save.nvim",    opts = { execution_message = { message = function() return "" end } } },
        { "nvimtools/hydra.nvim",      dependencies = { "anuvyklack/keymap-layer.nvim" } },
        { "chrisgrieser/nvim-genghis", dependencies = { "stevearc/dressing.nvim" } },

        -- Editing
        {
            "echasnovski/mini.nvim",
            dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
        },
        { "ap/vim-you-keep-using-that-word" },
        { "Konfekt/vim-CtrlXA",             dependencies = { "tpope/vim-repeat" } },
        {
            "numToStr/Comment.nvim",
            opts = {
                toggler = { line = ',cc', block = nil },
                opleader = { line = ',c', block = ',b' },
                extra = { above = ',cO', below = ',co', eol = ',cA' },
                mappings = { basic = true, extra = true },
            }
        },
        { "kana/vim-niceblock" },

        -- Search/Replace
        { "junegunn/vim-slash" },
        {
            "folke/flash.nvim",
            opts = {
                labels = "tnseriaodhgjplfuwybkvmcxzq",
                label = { uppercase = false, rainbow = { enabled = true } },
                jump = { nohlsearch = true },
                modes = { search = { enabled = false }, char = { enabled = false } },
            }
        },

        -- Git
        {
            "lewis6991/gitsigns.nvim",
            dependencies = {
                { "sindrets/diffview.nvim", dependencies = { "kyazdani42/nvim-web-devicons" } },
                "tpope/vim-repeat",
            },
            config = function() require("user.git") end
        },

        -- UI
        { "rebelot/kanagawa.nvim" },
        { "stevearc/dressing.nvim" },
        { "nvim-lualine/lualine.nvim",   dependencies = { "kyazdani42/nvim-web-devicons" } },
        { "luukvbaal/statuscol.nvim",    branch = "0.10" },
        { "nvim-zh/colorful-winsep.nvim" },
        {
            "https://gitlab.com/yorickpeterse/nvim-pqf",
            config = function() require('pqf').setup() end,
        },
        {
            "folke/edgy.nvim",
            config = function() require("user.panels") end,
        },
        {
            "folke/noice.nvim",
            dependencies = { "MunifTanjim/nui.nvim", "rcarriga/nvim-notify" },
            config = function() require("user.noice") end,
        },
        { "lukas-reineke/indent-blankline.nvim" },
        { "HiPhish/rainbow-delimiters.nvim" },

        {
            "folke/todo-comments.nvim",
            dependencies = { "nvim-lua/plenary.nvim" },
            opts = {
                signs = true,
                sign_priority = 2,
                keywords = {
                    FIX = { icon = " ", color = "error", alt = { "FIXME", "BUG", "FIXIT", "FIX", "ISSUE" } },
                    TODO = { icon = " ", color = "info" },
                    HACK = { icon = " ", color = "warning", alt = { "JANK", "WORKAROUND" } },
                    WARN = { icon = " ", color = "warning", alt = { "WARNING" } },
                    PERF = { icon = " ", alt = { "OPTIM", "PERFORMANCE", "OPTIMIZE" } },
                    NOTE = { icon = " ", color = "hint", alt = { "INFO" } },
                },
                highlight = {
                    before = "",
                    keyword = "bg",
                    after = "bg",
                    pattern = [[.*<(KEYWORDS)(\(.{-}\))?\s*:]]
                },
                colors = {
                    error = { "LspDiagnosticsDefaultError", "ErrorMsg", "#DC2626" },
                    warning = { "LspDiagnosticsDefaultWarning", "WarningMsg", "#FBBF24" },
                    info = { "LspDiagnosticsDefaultInformation", "#2563EB" },
                    hint = { "LspDiagnosticsDefaultHint", "#10B981" },
                    default = { "Identifier", "#7C3AED" },
                },

            }
        },

        -- lsp
        {
            "neovim/nvim-lspconfig",
            dependencies = {
                { "mrcjkb/rustaceanvim" },
                { "yioneko/nvim-type-fmt" },
                { "https://git.sr.ht/~whynothugo/lsp_lines.nvim" },
                { "folke/neodev.nvim" },
                { "MrcJkb/haskell-tools.nvim" },
                { "hrsh7th/cmp-nvim-lsp" },
                {
                    "williamboman/mason.nvim",
                    dependencies = {
                        "williamboman/mason-lspconfig.nvim",
                        "jayp0521/mason-nvim-dap.nvim",
                        "WhoIsSethDaniel/mason-tool-installer.nvim",
                    }
                },
                {
                    "nvimtools/none-ls.nvim",
                    dependencies = { "nvim-lua/plenary.nvim" },
                },

            },
            config = function() require("user.lsp") end
        },

        -- debug
        {
            "mfussenegger/nvim-dap",
            config = function() require("user.dap") end,
            dependencies = {
                {
                    "rcarriga/nvim-dap-ui",
                    dependencies = {
                        { "nvim-neotest/nvim-nio" },
                    },
                },
                { "theHamsta/nvim-dap-virtual-text" },
                { "mfussenegger/nvim-dap-python" },
            }
        },

        -- lang specific
        { "lervag/vimtex",              dependencies = { "tpope/vim-repeat" } },
        { "barreiroleo/ltex_extra.nvim" },
        { "fladson/vim-kitty" },
        { "wilriker/gcode.vim" },
        { "LhKipp/nvim-nu" },
        { "stsewd/sphinx.nvim",         build = ":UpdateRemotePlugins" },

        -- compleation
        {
            "hrsh7th/nvim-cmp",
            dependencies = {
                { "onsails/lspkind.nvim" },
                { "hrsh7th/cmp-buffer" },
                { "hrsh7th/cmp-path" },
                { "hrsh7th/cmp-nvim-lsp" },
                { "hrsh7th/cmp-cmdline" },
                { "dmitmel/cmp-cmdline-history" },
                { "micangl/cmp-vimtex" },
                { "saadparwaiz1/cmp_luasnip" },
                { "L3MON4D3/cmp-luasnip-choice" },
                { "kdheepak/cmp-latex-symbols" },
                { "rcarriga/cmp-dap" },
            },
            config = function() require("user.compleation") end,
        },
        {
            "L3MON4D3/LuaSnip",
            dependencies = {
                { "iurimateus/luasnip-latex-snippets.nvim" },
            },
            config = function()
                require("luasnip").config.set_config({
                    history = false,
                    update_events = "TextChanged,TextChangedI",
                    delete_check_events = "TextChanged",
                })
                require("luasnip.loaders.from_lua").load({ paths = "/home/oleete/.config/nvim/snippets" })
                require("luasnip-latex-snippets").setup()
            end

        },

        -- telescope
        {
            "nvim-telescope/telescope.nvim",
            dependencies = {
                { "nvim-lua/plenary.nvim" },
                { "kyazdani42/nvim-web-devicons" },
                {
                    "nvim-telescope/telescope-fzf-native.nvim",
                    build = [[
                        cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release &&
                        cmake --build build --config Release &&
                        cmake --install build --prefix build
                    ]],
                },
                { "prochri/telescope-all-recent.nvim", dependencies = { "kkharji/sqlite.lua" } },
            },
            config = function() require("user.telescope") end
        },
        {
            "FeiyouG/commander.nvim",
            config = function() require("user.command") end,
            keys = { { "<leader>p", function() require("commander").show() end } }
        },

        -- treesitter
        {
            "nvim-treesitter/nvim-treesitter",
            build = { ":TSInstall all", ":TSUpdate all", ":TSUninstall comment" },
            dependencies = {
                { "ThePrimeagen/refactoring.nvim" },
                { "CKolkey/ts-node-action",       dependencies = { "tpope/vim-repeat" } },
                { "drybalka/tree-climber.nvim" },
            },
            config = function()
                require("user.treesitter")
            end,
        },
    },
    {
        checker = {
            enabled = true,
        },
        install = {
            missing = true,
        },
        ui = {
            border = Border,
            title = " Lazy ",
        },
        dev = {
            path = "~/Projects",
        },
    }
)

require("user.mappings")
require("user.myfuncs")
require("user.mini")
require("user.targets")

require("user.filmpicker")
require("user.ui")
require("user.statusline")
require("user.statuscol")
