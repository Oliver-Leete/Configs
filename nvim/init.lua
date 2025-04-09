vim.api.nvim_set_var("$SHELL", "/bin/zsh")
vim.opt.shell = "/bin/zsh"

local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
    if vim.v.shell_error ~= 0 then
        vim.api.nvim_echo({
            { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
            { out,                            "WarningMsg" },
            { "\nPress any key to exit..." },
        }, true, {})
        vim.fn.getchar()
        os.exit(1)
    end
end
vim.opt.rtp:prepend(lazypath)

require("config.options")
require("config.autocmds")
require("config.commands")
require("config.keymaps")

require("lazy").setup(
    {
        spec = {
            { import = "plugins" },
            { import = "plugins.langs" },
            { import = "plugins.editor" },
            { import = "plugins.coding" },
        },
        checker = {
            enabled = true,
            check_pinned = true,
        },
        install = {
            missing = true,
        },
        ui = {
            border = vim.o.winborder,
            title = " Lazy ",
        },
        ---@diagnostic disable-next-line: assign-type-mismatch
        dev = {
            path = "~/Projects/nvim",
        },
        performance = {
            rtp = {
                disabled_plugins = {
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
            }
        }
    }
)

require("user.targets")
