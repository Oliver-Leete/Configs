local enterAndExitVim = vim.api.nvim_create_augroup("enterAndExitVim", { clear = true })
vim.api.nvim_create_autocmd("VimEnter",
    { command = 'silent! !/home/oleete/.config/bin/nvrRename', group = enterAndExitVim })
vim.api.nvim_create_autocmd("VimLeave", { command = 'silent! !kitty @ set-window-title ""', group = enterAndExitVim })

vim.g.matchup_matchparen_offscreen = { method = false }
vim.g.matchup_text_obj_enabled = false
vim.g.matchup_mouse_enabled = false

vim.api.nvim_create_autocmd('BufReadPost', {
  callback = function()
    local mark = vim.api.nvim_buf_get_mark(0, '"')
    local lcount = vim.api.nvim_buf_line_count(0)
    if mark[1] > 0 and mark[1] <= lcount then
      pcall(vim.api.nvim_win_set_cursor, 0, mark)
    end
  end,
})

require("hop").setup({ keys = "tnseriaodhgjplfuwybkvmcxzq" })

require("hex").setup()

local util = require("perfanno.util")

require("perfanno").setup({
    line_highlights = util.make_bg_highlights("#1F1F28", "#C34043", 10),
    vt_highlight = util.make_fg_highlights("#1F1F28", "#C34043", 10),
    formats = {
        { percent = true, format = "%.2f%%", minimum = 0.0 },
        { percent = false, format = "%d", minimum = 0.0001 },
    },
})
require("coverage").setup({
    signs = {
        covered = { hl = "CoverageCovered", text = "▉" },
        uncovered = { hl = "CoverageUncovered", text = "▉" },
    },
})

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

-- require("ssr").setup({})
