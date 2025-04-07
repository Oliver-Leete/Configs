vim.api.nvim_set_var("$SHELL", "/bin/zsh")
vim.opt.shell = "/bin/zsh"

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

require("config.options")

require("lazy").setup(
    {
        spec = {
            { import = "plugins" },
            { import = "plugins.langs" },
            { import = "plugins.editor" },
            { import = "plugins.coding" },
        },
        checker = {
            enabled = false,
        },
        install = {
            missing = true,
        },
        ui = {
            border = require("config.options").border,
            title = " Lazy ",
        },
        dev = {
            path = "~/Projects/nvim",
        },
    }
)

require("config.autocmds")
require("config.commands")
require("config.keymaps")
require("user.targets")
