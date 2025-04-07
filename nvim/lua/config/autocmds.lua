local user_autocmds = vim.api.nvim_create_augroup("user_autocmds", { clear = true })

vim.api.nvim_create_autocmd("FileType", {
    group = user_autocmds,
    pattern = "help",
    command = ":wincmd H | vertical resize 90<cr>"
})
vim.api.nvim_create_autocmd("FileType", {
    group = user_autocmds,
    pattern = "qf",
    command = "wincmd J"
})

vim.api.nvim_create_autocmd("TextYankPost", {
    group = user_autocmds,
    callback = function() vim.hl.on_yank({ higroup = "Visual", timeout = 200 }) end,
})

vim.api.nvim_create_autocmd("BufRead", {
    group = user_autocmds,
    pattern = "/tmp/film_list.films",
    callback = function()
        vim.keymap.set("n", "<leader>a", "vip:!sort -k1<cr><cr>")
        vim.keymap.set("n", "<leader>r", "vip:!sort -k3 -h<cr><cr>")
        vim.keymap.set("n", "<leader>s", "vip:!sort -k5<cr><cr>")
    end,
})
