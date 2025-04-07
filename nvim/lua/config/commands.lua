vim.api.nvim_create_user_command("NavigateLeft", function() require("user.myfuncs").nav_dir("h") end, { nargs = 0 })
vim.api.nvim_create_user_command("NavigateBottom", function() require("user.myfuncs").nav_dir("j") end, { nargs = 0 })
vim.api.nvim_create_user_command("NavigateTop", function() require("user.myfuncs").nav_dir("k") end, { nargs = 0 })
vim.api.nvim_create_user_command("NavigateRight", function() require("user.myfuncs").nav_dir("l") end, { nargs = 0 })

vim.api.nvim_create_user_command("DeleteBuffer", function() require("user.myfuncs").delete_buffer() end, { nargs = 0 })
