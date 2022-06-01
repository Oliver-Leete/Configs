Map("n", "<localleader>h", "<cmd>Telescope heading<cr>", { buffer = 0 })
Map("n", "<localleader><localleader>", "<plug>MarkdownPreviewToggle", { buffer = 0 })
Map("n", ",fw", "m1!ippar w80<cr>`1", { silent = true, buffer = 0 })

vim.api.nvim_buf_set_option(0, "textwidth", 80)
