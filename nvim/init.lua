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

vim.api.nvim_set_var("$SHELL", "/bin/zsh")
vim.opt.shell = "/bin/zsh"

vim.cmd([[call plug#begin('~/.config/nvim/pluged')
    Plug 'nvim-lua/plenary.nvim'
    Plug 'tpope/vim-repeat'
    Plug 'kyazdani42/nvim-web-devicons'
    Plug '907th/vim-auto-save'

    Plug 'stevearc/dressing.nvim'
    Plug 'rcarriga/nvim-notify'

    Plug 'knubie/vim-kitty-navigator'
    Plug 'kazhala/close-buffers.nvim'
    Plug 'tpope/vim-projectionist'
    Plug 'ThePrimeagen/harpoon'

    Plug 'ap/vim-you-keep-using-that-word'
    Plug 'phaazon/hop.nvim'
    Plug 'chaoren/vim-wordmotion'
    Plug 'junegunn/vim-slash'
    Plug 'numToStr/Comment.nvim'
    Plug 'arthurxavierx/vim-caser'
    Plug 'junegunn/vim-easy-align'
    Plug 'christoomey/vim-sort-motion'
    Plug 'Konfekt/vim-CtrlXA'
    Plug 'svermeulen/vim-subversive'
    Plug 'inkarkat/vim-unconditionalpaste'
    Plug 'tpope/vim-abolish'
    Plug 'tpope/vim-surround'
    Plug 'andymass/vim-matchup'

    Plug 'tommcdo/vim-ninja-feet'
    Plug 'wellle/targets.vim'
    Plug 'wellle/line-targets.vim'

    Plug 'lervag/vimtex'
    Plug 'JuliaEditorSupport/julia-vim'
    Plug 'fladson/vim-kitty'
    Plug 'iamcco/markdown-preview.nvim', { 'do': { -> mkdp#util#install() }, 'for': ['markdown', 'vim-plug']}
    Plug 'wilriker/gcode.vim'

    Plug 'lewis6991/gitsigns.nvim'
    Plug 'sindrets/diffview.nvim'
    Plug 'akinsho/git-conflict.nvim'

    Plug 'rebelot/kanagawa.nvim'
    Plug 'norcalli/nvim-colorizer.lua'
    Plug 'windwp/windline.nvim'
    Plug 'folke/zen-mode.nvim'

    Plug 'mbbill/undotree'
    Plug 'kyazdani42/nvim-tree.lua'
    Plug 'folke/todo-comments.nvim'

    Plug 'neovim/nvim-lspconfig'
    Plug 'williamboman/nvim-lsp-installer'
    Plug 'brymer-meneses/grammar-guard.nvim'
    Plug 'simrat39/rust-tools.nvim'
    Plug 'p00f/clangd_extensions.nvim'
    Plug 'b0o/schemastore.nvim'
    Plug 'jose-elias-alvarez/null-ls.nvim'

    Plug 'ray-x/lsp_signature.nvim'
    Plug 'SmiteshP/nvim-navic'

    Plug 'hrsh7th/nvim-cmp'
    Plug 'hrsh7th/cmp-buffer'
    Plug 'hrsh7th/cmp-path'
    Plug 'hrsh7th/cmp-nvim-lua'
    Plug 'hrsh7th/cmp-nvim-lsp'
    Plug 'hrsh7th/cmp-omni'
    Plug 'petertriho/cmp-git'
    Plug 'hrsh7th/cmp-cmdline'
    Plug 'dmitmel/cmp-cmdline-history'
    Plug 'uga-rosa/cmp-dictionary'
    Plug 'mtoohey31/cmp-fish'
    Plug 'saadparwaiz1/cmp_luasnip'

    Plug 'L3MON4D3/LuaSnip'
    Plug 'danymat/neogen'
    Plug 'windwp/nvim-autopairs'

    Plug 'nvim-telescope/telescope-file-browser.nvim'
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make'}
    Plug 'crispgm/telescope-heading.nvim'
    Plug 'gbrlsnchs/telescope-lsp-handlers.nvim'

    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    Plug 'nvim-treesitter/playground'
    Plug 'nvim-treesitter/nvim-treesitter-textobjects'
    Plug 'ThePrimeagen/refactoring.nvim'
    Plug 'mizlan/iswap.nvim'
    Plug 'AckslD/nvim-trevJ.lua'

    Plug 'mfussenegger/nvim-dap'
    Plug 'Pocco81/DAPInstall.nvim'
    Plug 'rcarriga/nvim-dap-ui'
    Plug 'theHamsta/nvim-dap-virtual-text'
    Plug 'nvim-telescope/telescope-dap.nvim'

    Plug 't-troebst/perfanno.nvim'
    Plug 'andythigpen/nvim-coverage'

    Plug 'akinsho/toggleterm.nvim'
call plug#end()]])

-- Disable builtins
local disabled_built_ins = {
    "netrw",
    "netrwPlugin",
    "netrwSettings",
    "netrwFileHandlers",
    "gzip",
    "zip",
    "zipPlugin",
    "tar",
    "tarPlugin",
    "getscript",
    "getscriptPlugin",
    "vimball",
    "vimballPlugin",
    "2html_plugin",
    "logipat",
    "rrhelper",
    "spellfile_plugin",
    "matchit",
}

for _, plugin in pairs(disabled_built_ins) do
    vim.g["loaded_" .. plugin] = 1
end

require("settings_config")
require("misc_config")
require("mappings_config")
require("jump_config")
require("telescope_config")
require("compleation_config")
require("panels_config")
require("myfuncs_config")
require("lsp_config")
require("treesitter_config")
require("dap_config")
require("bubble")
require("ui_config")
require("git_config")
require("terminal_config")
require("projects_config")

local enterAndExitVim = vim.api.nvim_create_augroup("enterAndExitVim", { clear = true })
vim.api.nvim_create_autocmd("VimLeave", { command = 'silent! !kitty @ set-window-title ""', group = enterAndExitVim })

-- vim.api.nvim_create_autocmd("VimEnter",
--     { command = 'silent! !kitty @ set-colors background=\\#262626', group = enterAndExitVim })
-- vim.api.nvim_create_autocmd("VimLeave",
--     { command = 'silent! !kitty @ set-colors background=\\#1F1F28', group = enterAndExitVim })

local qfDiag = vim.api.nvim_create_namespace("qfDiag")
local qfToDiag = vim.api.nvim_create_augroup("qfToDiag", { clear = true })

local function UpdateDiagnostics(diagnostics, namespace)
    vim.diagnostic.reset(namespace)
    local buffers = {}
    local tmp = {}
    for i, item in pairs(diagnostics) do
        if (tmp[item.bufnr] ~= nil) then
            table.insert(buffers, item.bufnr)
        end
        tmp[item.bufnr] = i
    end

    for _, buffer in pairs(buffers) do
        local diag = {}
        for _, d in pairs(diagnostics) do
            if d.bufnr == buffer then
                table.insert(diag, d)
            end
        end
        vim.diagnostic.set(namespace, buffer, diag)
    end
end

QFtoDiag = function()
    local qf = vim.diagnostic.fromqflist(vim.fn.getqflist())
    UpdateDiagnostics(qf, qfDiag)
end
vim.api.nvim_create_autocmd("QuickFixCmdPost", { pattern = "*", callback = QFtoDiag, group = qfToDiag })
vim.api.nvim_create_autocmd("User", { pattern = "VimtexEventCompileFailed", callback = QFtoDiag, group = qfToDiag })
vim.api.nvim_create_autocmd("User", { pattern = "VimtexEventCompileSuccess", callback = QFtoDiag, group = qfToDiag })
