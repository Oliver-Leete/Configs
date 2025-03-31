-- Leader Mapping
vim.opt.timeoutlen = 500

-- Un-Mappings
vim.keymap.set({ "n", "x", "o" }, "<backspace>", "<nop>")
vim.keymap.set({ "n", "x", "o" }, "<space>", "<nop>")

vim.keymap.set({ "n", "x", "o" }, ",", "<nop>")
vim.keymap.set({ "n", "x", "o" }, ";", "<nop>")
vim.keymap.set({ "n", "x", "o" }, "<M-n>", ";")
vim.keymap.set({ "n", "x", "o" }, "<M-N>", ",")

vim.keymap.set({ "n", "x", "o" }, "v", "<nop>")
vim.keymap.set({ "n", "x", "o" }, "V", "<nop>")
vim.keymap.set({ "n", "x", "o" }, "<c-v>", "<nop>")
vim.keymap.set({ "n", "x", "o" }, "dd", "<nop>")
vim.keymap.set({ "n", "x", "o" }, "cc", "<nop>")
vim.keymap.set({ "n", "x", "o" }, "yy", "<nop>")
vim.keymap.set({ "n", "x", "o" }, "L", "<nop>")

vim.keymap.set({ "n", "x", "o" }, "Y", "<nop>")
vim.keymap.set({ "n", "x", "o" }, "C", "<nop>")
vim.keymap.set({ "n", "x", "o" }, "D", "<nop>")
vim.keymap.set({ "n", "x", "o" }, "S", "<nop>")
vim.keymap.set({ "n", "x", "o" }, "G", "<nop>")
vim.keymap.set({ "n", "x" }, "H", "<nop>")


vim.keymap.set({ "n", "x", "o" }, "$", "<nop>")
vim.keymap.set({ "n", "x", "o" }, "^", "<nop>")

vim.keymap.set({ "n", "x", "o" }, "q:", "<nop>")
vim.keymap.set({ "n", "x", "o" }, "q/", "<nop>")
vim.keymap.set({ "n", "x", "o" }, "q?", "<nop>")
vim.keymap.set("c", "<c-f>", "<nop>")

vim.keymap.set({ "n", "x", "o" }, "(", "<nop>")
vim.keymap.set({ "n", "x", "o" }, ")", "<nop>")
-- NOTE: D, Y, H, L, £, _, =, |, ;, ^, <BS>, <CR> are free to map
-- NOTE: H and L are free except op mode
-- NOTE: y, d, c are free in op mode

-- Mappings

vim.keymap.set({ "n", "x" }, "<c-r>", "<c-x>")
vim.keymap.set({ "n", "x" }, "g<c-r>", "g<c-x>")

vim.keymap.set("x", "y", "m1y`1", { nowait = true })
vim.keymap.set("x", "d", "d", { nowait = true })
vim.keymap.set("x", "c", "c", { nowait = true })

vim.keymap.set("n", "x", "V")
vim.keymap.set("n", "X", "V")
vim.keymap.set("n", "C", "<c-v>j")
vim.keymap.set("n", "<m-C>", "<c-v>k")
vim.keymap.set("n", "<m-C>", "<c-v>k")

vim.keymap.set("x", "x", "j$")
vim.keymap.set("x", "X", "<esc>`<kV`>")
vim.keymap.set("x", "C", "j")
vim.keymap.set("x", "<m-C>", "<esc>`<k<c-v>`>")

vim.keymap.set("n", "<m-o>", "m1o<esc>`1")
vim.keymap.set("n", "<m-O>", "m1O<esc>`1")
vim.keymap.set("x", "<m-o>", "<esc>`>o<esc>gv")
vim.keymap.set("x", "<m-O>", "<esc>`<O<esc>gv")

vim.keymap.set("x", "I", "<Plug>(niceblock-I)")
vim.keymap.set("x", "A", "<Plug>(niceblock-A)")

vim.keymap.set("n", "<c-/>", ",cc", { remap = true })
vim.keymap.set("x", "<c-/>", ",c", { remap = true })

-- UnMap Plugins
vim.g.vimtex_mappings_enabled = 0
vim.g.vimtex_text_obj_enabled = 0
vim.g.vimtex_imaps_enabled = 0

vim.keymap.set({ "n", "x", "o" }, "j", [[v:count?(v:count>5?"m'".v:count:'').'j':'gj']], { expr = true })
vim.keymap.set({ "n", "x", "o" }, "k", [[v:count?(v:count>5?"m'".v:count:'').'k':'gk']], { expr = true })

vim.keymap.set("o", "H", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], { expr = true })
vim.keymap.set("o", "L", "$")

vim.keymap.set({ "n", "x" }, "u", function()
    pcall(Ls.unlink_current)
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("u", true, true, true), "n", false)
end)
vim.keymap.set({ "n", "x" }, "U", "<c-r>")

vim.keymap.set("n", "Q", "@q")
vim.keymap.set("x", "Q", ":norm! @q<cr>")


vim.keymap.set("x", "<", "<gv")
vim.keymap.set("x", ">", ">gv")

vim.keymap.set("x", "/", "<esc>/\\%V")

vim.keymap.set("n", "<c-v>", "<cmd>silent vsplit<cr>")
vim.keymap.set("n", "<c-x>", "<cmd>silent split<cr>")
vim.keymap.set("n", "<c-t>", "<cmd>silent tabedit %<cr>")

-- GOTO
vim.keymap.set({ "n", "x", "o" }, "gk", "gg", { desc = "Top of File" })
vim.keymap.set({ "n", "x", "o" }, "gj", "G", { desc = "Bottom of File" })
vim.keymap.set({ "n", "x", "o" }, "gh", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], { expr = true },
    { desc = "Left of Line" })
vim.keymap.set({ "n", "x", "o" }, "gl", "$", { desc = "Right of Line" })

vim.keymap.set({ "n", "x", "o" }, "gt", "H", { desc = "Top of Screen" })
vim.keymap.set({ "n", "x", "o" }, "gm", "M", { desc = "Middle of Screen" })
vim.keymap.set({ "n", "x", "o" }, "gb", "L", { desc = "Bottom of Screen" })

vim.keymap.set({ "n", "x", "o" }, "gV", "`[v`]")
vim.keymap.set("n", "gF", ":edit <cfile><cr>")
vim.keymap.set("n", "gx", function() vim.ui.open(vim.fn.expand("<cfile>")) end)

vim.keymap.set(
    { "n", "x" },
    "gz",
    function()
        if vim.b.upafunc then
            vim.b.upafunc()
        else
            vim.notify("No up function found for this filetype",
                vim.log.levels.WARN)
        end
    end,
    { desc = "Go to the parent file" }
)

vim.keymap.set("n", "vH", "zH", { desc = "⇚" })
vim.keymap.set("n", "vJ", "<c-d>", { desc = "⟱" })
vim.keymap.set("n", "vK", "<c-u>", { desc = "⤊" })
vim.keymap.set("n", "vL", "zL", { desc = "⇛" })
vim.keymap.set("n", "vh", "zh", { desc = "←" })
vim.keymap.set("n", "vj", "<c-e>", { desc = "↑" })
vim.keymap.set("n", "vk", "<c-y>", { desc = "↑" })
vim.keymap.set("n", "vl", "zl", { desc = "→" })
vim.keymap.set("n", "vt", "zt", { desc = "Jump to top" })
vim.keymap.set("n", "vv", "zz", { desc = "Jump to middle" })
vim.keymap.set("n", "vb", "zb", { desc = "Jump to bottom" })
vim.keymap.set("n", "vs", "zs", { desc = "Jump to start" })
vim.keymap.set("n", "vm", "<cmd>set sidescrolloff=999<cr>hl<cmd>set sidescrolloff=0<cr>", { desc = "Jump to middle" })
vim.keymap.set("n", "ve", "ze", { desc = "Jump to end" })
vim.keymap.set("n", "vo", "<c-w>o", { desc = "Only Window" })

vim.keymap.set({ "n", "x" }, "m", "v")

-- NOTE: Text leader mappings: ,
-- NOTE: text leader mappings

vim.keymap.set("n", ",rr", vim.lsp.buf.rename)

vim.keymap.set("n", ",n", require("ts-node-action").node_action)

vim.keymap.set({ "n", "x" }, ",ff", function()
    pcall(Ls.unlink_current)
    require("conform").format()
end)

local num = function() return (vim.b.textwidth and vim.b.textwidth > 0) and vim.b.textwidth or vim.g.textwidth end
vim.keymap.set("n", ",fw", function() return "m1!ippar w" .. num() .. "<cr>`1" end, { expr = true, silent = true })
vim.keymap.set("x", ",fw", function() return "!par w" .. num() .. "<cr>" end, { expr = true, silent = true })

vim.keymap.set("n", "<leader><leader>", "<cmd>silent e #<cr>", {desc = "Last file"})

-- Mouse Bindings

vim.keymap.set("n", "<c-rightmouse>", "gf")

-- Insert Bindings

vim.cmd([[inoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[inoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[snoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[snoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])

vim.keymap.set({ "i", "s", "c" }, "<c-a>", "<HOME>")
vim.keymap.set({ "i", "s", "c" }, "<c-e>", "<END>")
vim.keymap.set({ "i", "s" }, "<c-k>", "<c-o>d$")
vim.keymap.set("c", "<c-p>", "<up>")
vim.keymap.set("c", "<c-n>", "<down>")

local cmp = require("cmp")
vim.keymap.set({ "i", "s" }, "<c-]>", "<plug>luasnip-next-choice")
vim.keymap.set({ "i", "s", "c" }, "<c-space>", function() if cmp.visible() then cmp.close() else cmp.complete() end end)

Ls = require("luasnip")
vim.keymap.set({ "i", "s" }, "<tab>", function()
    if Ls.expand_or_locally_jumpable() then
        Ls.expand_or_jump()
    else
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<tab>", true, true, true), "n", false)
    end
end, { silent = true })

vim.keymap.set({ "i", "s" }, "<s-tab>", function()
    if Ls.locally_jumpable(-1) then
        Ls.jump(-1)
    else
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<s-tab>", true, true, true), "n", false)
    end
end, { silent = true })

vim.keymap.set("i", "<c-n>", function()
    if Ls.choice_active() then
        Ls.change_choice(1)
    end
end)

-- Terminal Bindings
vim.keymap.set("t", "<c-]>", "<c-\\><c-n>")
