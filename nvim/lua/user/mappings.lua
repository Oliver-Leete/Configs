-- Leader Mapping
vim.opt.timeoutlen = 500

-- Un-Mappings
Map({ "n", "x", "o" }, "<BackSPACE>", "<Nop>")
Map({ "n", "x", "o" }, "<SPACE>", "<Nop>")

Map({ "n", "x", "o" }, ",", "<Nop>")
Map({ "n", "x", "o" }, ";", "<Nop>")
Map({ "n", "x", "o" }, "<M-n>", ";")
Map({ "n", "x", "o" }, "<M-N>", ",")

Map({ "n", "x", "o" }, "v", "<nop>")
Map({ "n", "x", "o" }, "V", "<nop>")
Map({ "n", "x", "o" }, "<c-v>", "<nop>")
Map({ "n", "x", "o" }, "dd", "<nop>")
Map({ "n", "x", "o" }, "cc", "<nop>")
Map({ "n", "x", "o" }, "yy", "<nop>")
Map({ "n", "x", "o" }, "L", "<nop>")

Map({ "n", "x", "o" }, "Y", "<nop>")
Map({ "n", "x", "o" }, "C", "<nop>")
Map({ "n", "x", "o" }, "D", "<nop>")
Map({ "n", "x", "o" }, "S", "<nop>")
Map({ "n", "x", "o" }, "G", "<nop>")
Map({ "n", "x", "o" }, "K", "<nop>")
Map({ "n", "x" }, "H", "<nop>")


Map({ "n", "x", "o" }, "$", "<nop>")
Map({ "n", "x", "o" }, "^", "<nop>")

Map({ "n", "x", "o" }, "q:", "<nop>")
Map({ "n", "x", "o" }, "q/", "<nop>")
Map({ "n", "x", "o" }, "q?", "<nop>")
Map("c", "<c-f>", "<nop>")

Map({ "n", "x", "o" }, "(", "<nop>")
Map({ "n", "x", "o" }, ")", "<nop>")
-- NOTE: D, Y, H, L, £, _, =, |, ;, ^, <BS>, <CR> are free to map
-- NOTE: H and L are free except op mode
-- NOTE: y, d, c are free in op mode

-- Mappings

Map({ "n", "x" }, "<c-r>", "<c-x>")
Map({ "n", "x" }, "g<c-r>", "g<c-x>")

Map("x", "y", "m1y`1", { nowait = true })
Map("x", "d", "d", { nowait = true })
Map("x", "c", "c", { nowait = true })

Map("n", "x", "V")
Map("n", "X", "V")
Map("n", "C", "<c-v>j")
Map("n", "<m-C>", "<c-v>k")
Map("n", "<m-C>", "<c-v>k")

Map("x", "x", "j$")
Map("x", "X", "<esc>`<kV`>")
Map("x", "C", "j")
Map("x", "<m-C>", "<esc>`<k<c-v>`>")

Map("n", "<m-o>", "m1o<esc>`1")
Map("n", "<m-O>", "m1O<esc>`1")
Map("x", "<m-o>", "<esc>`>o<esc>gv")
Map("x", "<m-O>", "<esc>`<O<esc>gv")

Map("x", "I", "<Plug>(niceblock-I)")
Map("x", "A", "<Plug>(niceblock-A)")

Map("n", "<c-/>", ",cc", { remap = true })
Map("x", "<c-/>", ",c", { remap = true })

-- UnMap Plugins
vim.g.vimtex_mappings_enabled = 0
vim.g.vimtex_text_obj_enabled = 0
vim.g.vimtex_imaps_enabled = 0

Map({ "n", "x", "o" }, "j", [[v:count?(v:count>5?"m'".v:count:'').'j':'gj']], { expr = true })
Map({ "n", "x", "o" }, "k", [[v:count?(v:count>5?"m'".v:count:'').'k':'gk']], { expr = true })

Map("o", "H", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], { expr = true })
Map("o", "L", "$")

Map({ "n", "x" }, "u", function()
    pcall(Ls.unlink_current)
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("u", true, true, true), "n", false)
end)
Map({ "n", "x" }, "U", "<c-r>")

Map({ "n", "x" }, "KK", vim.lsp.buf.hover, { desc = "Info" })
Map({ "n", "x" }, "KE",
    function() vim.diagnostic.open_float({ border = require("user.settings").border, scope = "line", source = "always" }) end,
    { desc = "Errors" })
Map({ "n", "x" }, "KG", function() require("mini.diff").toggle_overlay() end, { desc = "Git Hunk" })
Map({ "n", "x" }, "KB", function() require("mini.git").show_at_cursor({ split = "vertical" }) end)
Map({ "n", "x" }, "KD", require("dap.ui.widgets").hover, { desc = "Test Results" })

Map("n", "Q", "@q")
Map("x", "Q", ":norm! @q<cr>")

Map({ "n", "x", "o" }, "s", require("flash").jump)
Map({ "n" }, "z", require("flash").treesitter)
Map({ "o" }, "z", require("flash").treesitter)
Map({ "o" }, "S", require("flash").remote)

Map("x", "<", "<gv")
Map("x", ">", ">gv")

Map("x", "/", "<esc>/\\%V")

Map("n", "<c-v>", "<cmd>silent vsplit<cr>")
Map("n", "<c-x>", "<cmd>silent split<cr>")
Map("n", "<c-t>", "<cmd>silent tabedit %<cr>")

-- GOTO
Map({ "n", "x", "o" }, "gk", "gg", { desc = "Top of File" })
Map({ "n", "x", "o" }, "gj", "G", { desc = "Bottom of File" })
Map({ "n", "x", "o" }, "gh", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], { expr = true },
    { desc = "Left of Line" })
Map({ "n", "x", "o" }, "gl", "$", { desc = "Right of Line" })

Map({ "n", "x", "o" }, "gt", "H", { desc = "Top of Screen" })
Map({ "n", "x", "o" }, "gm", "M", { desc = "Middle of Screen" })
Map({ "n", "x", "o" }, "gb", "L", { desc = "Bottom of Screen" })

Map({ "n", "x", "o" }, "gV", "`[v`]")
Map("n", "gF", ":edit <cfile><cr>")
Map("n", "gx", function() vim.ui.open(vim.fn.expand("<cfile>")) end)

Map({ "n", "x" }, "gz", function()
    if vim.b.upafunc then
        vim.b.upafunc()
    else
        vim.notify("No up function found for this filetype", vim.log.levels.WARN)
    end
end)

Map("n", "vH", "zH", { desc = "⇚" })
Map("n", "vJ", "<c-d>", { desc = "⟱" })
Map("n", "vK", "<c-u>", { desc = "⤊" })
Map("n", "vL", "zL", { desc = "⇛" })
Map("n", "vh", "zh", { desc = "←" })
Map("n", "vj", "<c-e>", { desc = "↑" })
Map("n", "vk", "<c-y>", { desc = "↑" })
Map("n", "vl", "zl", { desc = "→" })
Map("n", "vt", "zt", { desc = "Jump to top" })
Map("n", "vv", "zz", { desc = "Jump to middle" })
Map("n", "vb", "zb", { desc = "Jump to bottom" })
Map("n", "vs", "zs", { desc = "Jump to start" })
Map("n", "vm", "<cmd>set sidescrolloff=999<cr>hl<cmd>set sidescrolloff=0<cr>", { desc = "Jump to middle" })
Map("n", "ve", "ze", { desc = "Jump to end" })
Map("n", "vo", "<c-w>o", { desc = "Only Window" })

Map({ "n", "x" }, "m", "v")

Map("x", ',mj', function() require('mini.move').move_selection("down") end, { nowait = true })
Map("x", ',mh', function() require('mini.move').move_selection("left") end, { nowait = true })
Map("x", ',mk', function() require('mini.move').move_selection("up") end, { nowait = true })
Map("x", ',ml', function() require('mini.move').move_selection("right") end, { nowait = true })
Map("n", ',mh', function() require('mini.move').move_line("left") end, { nowait = true })
Map("n", ',mj', function() require('mini.move').move_line("down") end, { nowait = true })
Map("n", ',mk', function() require('mini.move').move_line("up") end, { nowait = true })
Map("n", ',ml', function() require('mini.move').move_line("right") end, { nowait = true })
-- NOTE: Text leader mappings: ,
-- NOTE: text leader mappings

Map("n", ",rr", vim.lsp.buf.rename)

Map("n", ",rf", function() require("refactoring").refactor("Extract Block") end)
Map("x", ",rf", function() require("refactoring").refactor("Extract Function") end)
Map("x", ",rF", require("genghis").moveSelectionToNewFile)
Map("n", ",re", "mia:lua require('refactoring').refactor('Extract Variable')<cr>", { remap = true })
Map("x", ",re", function() require("refactoring").refactor("Extract Variable") end)
Map({ "n", "x" }, ",ri", function() require("refactoring").refactor("Inline Variable") end)
Map("n", ",rI", function() require("refactoring").refactor("Inline Function") end)

Map("n", ",n", require("ts-node-action").node_action)

Map({ "n", "x" }, ",pl", function() require("user.myfuncs").paste_special(vim.v.register, "l", "p") end)
Map({ "n", "x" }, ",pi", function() require("user.myfuncs").paste_special(vim.v.register, "c", "p") end)

Map({ "n", "x" }, ",ff", function()
    pcall(Ls.unlink_current)
    vim.lsp.buf.format()
end)

local num = function() return (vim.b.textwidth and vim.b.textwidth > 0) and vim.b.textwidth or vim.g.textwidth end
Map("n", ",fw", function() return "m1!ippar w" .. num() .. "<cr>`1" end, { expr = true, silent = true })
Map("x", ",fw", function() return "!par w" .. num() .. "<cr>" end, { expr = true, silent = true })

Map("n", "<leader><leader>", "<cmd>silent e #<cr>")

Map("n", "<leader>m", function() require("trouble").toggle({ mode = "quickprev" }) end)
Map("n", "<leader>,", function() require("trouble").toggle({ mode = "lspprev" }) end)

Map("n", "<leader>s", function () require("namu.namu_symbols").show() end)

-- Mouse Bindings

Map("n", "<c-leftmouse>", "<cmd>Telescope lsp_definitions theme=get_ivy<cr>")
Map("n", "<c-rightmouse>", "gf")

-- Insert Bindings

vim.cmd([[inoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[inoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[snoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[snoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])

Map({ "i", "s", "c" }, "<c-a>", "<HOME>")
Map({ "i", "s", "c" }, "<c-e>", "<END>")
Map({ "i", "s" }, "<c-k>", "<c-o>d$")
Map("c", "<c-p>", "<up>")
Map("c", "<c-n>", "<down>")

local cmp = require("cmp")
Map({ "i", "s" }, "<c-]>", "<plug>luasnip-next-choice")
Map({ "i", "s", "c" }, "<c-space>", function() if cmp.visible() then cmp.close() else cmp.complete() end end)

Ls = require("luasnip")
Map({ "i", "s" }, "<tab>", function()
    if Ls.expand_or_locally_jumpable() then
        Ls.expand_or_jump()
    else
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<tab>", true, true, true), "n", false)
    end
end, { silent = true })

Map({ "i", "s" }, "<s-tab>", function()
    if Ls.locally_jumpable(-1) then
        Ls.jump(-1)
    else
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<s-tab>", true, true, true), "n", false)
    end
end, { silent = true })

Map("i", "<c-n>", function()
    if Ls.choice_active() then
        Ls.change_choice(1)
    end
end)

-- Terminal Bindings

Map("t", "<c-]>", "<c-\\><c-n>")

-- Git
Map("n", "<leader>gf", "<cmd>Telescope git_status theme=get_ivy<cr>", { desc = "Find git hunks" })


-- debuggin

local dapui = require("dapui")
local dap = require("dap")

Map("n", "<leader>d<", dap.up)
Map("n", "<leader>d>", dap.down)
Map("n", "<leader>dr", dap.continue, { desc = "continue" })
Map("n", "<leader>dR", dap.run_to_cursor)
Map("n", "<leader>dX", dap.terminate, { desc = "terminate" })
Map("n", "<leader>dp", dap.pause)
Map("n", "<leader>dj", function() dapui.toggle({ layout = 6 }) end)
Map("n", "<leader>dh", function() dapui.toggle({ layout = 4 }) end)
Map("n", "<leader>dk", function() dapui.toggle({ layout = 5 }) end)
Map("n", "<leader>dm", function() dapui.toggle({ layout = 3 }) end)
Map("n", "<leader>d,", function() dapui.toggle({ layout = 2 }) end)
Map("n", "<leader>d.", function() dapui.toggle({ layout = 1 }) end)
Map("n", "<leader>df", "<cmd>Telescope dap list_breakpoints theme=get_ivy<cr>")
