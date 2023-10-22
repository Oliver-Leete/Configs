vim.api.nvim_set_var("$SHELL", "/bin/zsh")
vim.opt.shell = "/bin/zsh"

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

pcall(require("user.settings"))

require("lazy").setup(
    {
        -- Misc
        { "907th/vim-auto-save" },
        {
            "anuvyklack/hydra.nvim",
            dependencies = { "anuvyklack/keymap-layer.nvim" }
        },
        {
            "chrisgrieser/nvim-genghis",
            dependencies = { "stevearc/dressing.nvim" }
        },
        { "stevearc/resession.nvim" },
        { "cbochs/grapple.nvim" },
        { "kazhala/close-buffers.nvim" },

        -- Editing
        {
            "echasnovski/mini.nvim",
            dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
        },
        { "ap/vim-you-keep-using-that-word" },
        {
            "ralismark/opsort.vim",
            dependencies = { "tpope/vim-repeat" }
        },
        {
            "Konfekt/vim-CtrlXA",
            dependencies = { "tpope/vim-repeat" }
        },
        { "andymass/vim-matchup" },
        { "numToStr/Comment.nvim" },
        { "kana/vim-niceblock" },

        -- Search/Replace
        { "junegunn/vim-slash" },
        { "folke/flash.nvim" },
        { "cshuaimin/ssr.nvim" },
        { "AckslD/muren.nvim" },

        -- Git
        {
            "lewis6991/gitsigns.nvim",
            dependencies = { "tpope/vim-repeat" }
        },
        {
            "sindrets/diffview.nvim",
            dependencies = { "kyazdani42/nvim-web-devicons" }
        },

        -- UI
        { "rebelot/kanagawa.nvim" },
        { "stevearc/dressing.nvim" },
        {
            "nvim-lualine/lualine.nvim",
            dependencies = { "kyazdani42/nvim-web-devicons" }
        },
        { "nvim-zh/colorful-winsep.nvim" },
        {
            "folke/noice.nvim",
            dependencies = { "MunifTanjim/nui.nvim", "rcarriga/nvim-notify" }
        },
        { "folke/edgy.nvim" },
        {
            "folke/todo-comments.nvim",
            dependencies = { "nvim-lua/plenary.nvim" }
        },
        {
            "folke/trouble.nvim",
            dependencies = { "kyazdani42/nvim-web-devicons" }
        },

        --
        {
            "williamboman/mason.nvim",
            dependencies = {
                "williamboman/mason-lspconfig.nvim",
                "jayp0521/mason-nvim-dap.nvim",
                "WhoIsSethDaniel/mason-tool-installer.nvim",
            }
        },
        {
            "jose-elias-alvarez/null-ls.nvim",
            dependencies = { "nvim-lua/plenary.nvim" }
        },
        { "neovim/nvim-lspconfig" },

        -- extra lsp
        { "SmiteshP/nvim-navic" },
        { "DNLHC/glance.nvim" },
        { "yioneko/nvim-type-fmt" },
        { "https://git.sr.ht/~whynothugo/lsp_lines.nvim" },
        { "johmsalas/text-case.nvim" },

        -- run and test
        { "stevearc/overseer.nvim" },
        {
            "nvim-neotest/neotest",
            dependencies = {
                { "rouge8/neotest-rust" },
                { "nvim-neotest/neotest-python" },
                { "mrcjkb/neotest-haskell" },
            },
        },
        { "andythigpen/nvim-coverage" },
        { "t-troebst/perfanno.nvim" },

        -- debug
        { "mfussenegger/nvim-dap" },
        { "rcarriga/nvim-dap-ui" },
        { "theHamsta/nvim-dap-virtual-text" },
        { "nvim-telescope/telescope-dap.nvim" },

        -- lang specific
        {
            "lervag/vimtex",
            dependencies = { "tpope/vim-repeat" }
        },
        { "iurimateus/luasnip-latex-snippets.nvim" },
        { "barreiroleo/ltex_extra.nvim" },
        {
            "toppair/peek.nvim",
            build = "deno task --quiet build:fast"
        },
        { "fladson/vim-kitty" },
        { "wilriker/gcode.vim" },
        { "LhKipp/nvim-nu" },
        { "simrat39/rust-tools.nvim" },
        { "mfussenegger/nvim-dap-python" },
        -- { "MrcJkb/haskell-tools.nvim" },
        { "folke/neodev.nvim" },

        -- compleation
        {
            "hrsh7th/nvim-cmp",
            dependencies = {
                { "onsails/lspkind.nvim" },
                { "hrsh7th/cmp-buffer" },
                { "hrsh7th/cmp-path" },
                { "hrsh7th/cmp-nvim-lua" },
                { "hrsh7th/cmp-nvim-lsp" },
                { "hrsh7th/cmp-omni" },
                { "petertriho/cmp-git" },
                { "hrsh7th/cmp-cmdline" },
                { "dmitmel/cmp-cmdline-history" },
                { "uga-rosa/cmp-dictionary" },
                { "saadparwaiz1/cmp_luasnip" },
                { "L3MON4D3/cmp-luasnip-choice" },
                { "kdheepak/cmp-latex-symbols" },
                { "rcarriga/cmp-dap" },
            }
        },
        {
            "L3MON4D3/LuaSnip",
            dependencies = { "tpope/vim-repeat" }
        },
        { "windwp/nvim-autopairs" },

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
                { "debugloop/telescope-undo.nvim" },
                { "FeiyouG/command_center.nvim" },
                { "Marskey/telescope-sg" },
            },
        },

        {},
        {},

        -- treesitter
        {
            "nvim-treesitter/nvim-treesitter",
            build = {
                ":TSInstall all",
                ":TSUpdate all",
                ":TSUninstall comment"
            },
        },
        { "ThePrimeagen/refactoring.nvim" },
        {
            "CKolkey/ts-node-action",
            dependencies = { "tpope/vim-repeat" }
        },
        { "drybalka/tree-climber.nvim" },
        { "Wansmer/treesj" },
        { "Wansmer/binary-Swap.nvim" },
        {
            "chrisgrieser/nvim-puppeteer",
            dependencies = "nvim-treesitter/nvim-treesitter",
        },
    },
    {
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

SendID = nil

pcall(require("user.misc"))
pcall(require("user.mappings"))
pcall(require("user.targets"))
pcall(require("user.telescope"))
pcall(require("user.compleation"))
pcall(require("user.myfuncs"))
pcall(require("user.lsp"))
pcall(require("user.treesitter"))
pcall(require("user.dap"))
pcall(require("user.panels"))

pcall(require("user.filmpicker"))
pcall(require("user.ui"))
pcall(require("user.statusline"))
pcall(require("user.tabline"))
pcall(require("user.statuscol"))

pcall(require("user.projects"))
pcall(require("user.overseer"))
pcall(require("user.testing"))

pcall(require("user.git"))
pcall(require("user.mini"))
pcall(require("user.noice"))

pcall(require("user.command"))

-- require("user.projects").load_session()
