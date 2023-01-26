-- if vim.b[0].buftype == "nofile" then
local bmap = function(mode, key, action) Map(mode, key, action, { buffer = 0 }) end
bmap({"n", "x"}, "KK", "<cmd>CEShowTooltip<cr>")
bmap({"n", "x"}, "gd", "<cmd>CEGotoLabel<cr>")
-- end
