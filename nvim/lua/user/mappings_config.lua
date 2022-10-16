Map = vim.keymap.set
local Hydra = require('hydra')

-- Leader Mapping
vim.opt.timeoutlen = 500
vim.api.nvim_set_var("mapleader", " ")
vim.api.nvim_set_var("maplocalleader", "\\")

-- Un-Mappings
Map({ "n", "x", "o" }, "<BackSPACE>", "<Nop>")
Map({ "n", "x", "o" }, "<SPACE>", "<Nop>")

Map({ "n", "x", "o" }, ",", "<Nop>")
Map({ "n", "x", "o" }, ";", "<Nop>")

Map({ "n", "x", "o" }, "v", "<nop>")
Map({ "n", "x", "o" }, "V", "<nop>")
Map({ "n", "x", "o" }, "<c-v>", "<nop>")
Map({ "n", "x", "o" }, "dd", "<nop>")
Map({ "n", "x", "o" }, "cc", "<nop>")
Map({ "n", "x", "o" }, "yy", "<nop>")
Map({ "n", "x", "o" }, "z", "<nop>")

Map({ "n", "x", "o" }, "Y", "<nop>")
Map({ "n", "x", "o" }, "C", "<nop>")
Map({ "n", "x", "o" }, "D", "<nop>")
Map({ "n", "x", "o" }, "S", "<nop>")

-- NOTE: _, =, |, ;, ^, <BS>, <CR> are free to map

-- Mappings
Map({ "n", "x", "o" }, "<m-f>", ";")
Map({ "n", "x", "o" }, "<m-F>", ",")
Map({ "n", "x", "o" }, "<m-t>", ";")
Map({ "n", "x", "o" }, "<m-T>", ",")

Map({ "n", "x" }, "+", "<c-a>")
Map({ "n", "x" }, "-", "<c-x>")
Map({ "n", "x" }, "g+", "g<c-a>")
Map({ "n", "x" }, "g-", "g<c-x>")

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

Map("x", "I", "I")
Map("x", "A", "A")

Map("n", "<c-/>", ",cc", { remap = true })
Map("x", "<c-/>", ",c", { remap = true })

Map("n", "£", [[:exe "let @/='" . expand("<cWORD>") . "' "<cr>]], { silent = true })

-- UnMap Plugins
vim.g.kitty_navigator_no_mappings = true
vim.g.julia_blocks = false

Map({ "n", "x", "o" }, "j", [[v:count?(v:count>5?"m'".v:count:'').'j':'gj']], { expr = true })
Map({ "n", "x", "o" }, "k", [[v:count?(v:count>5?"m'".v:count:'').'k':'gk']], { expr = true })

Map({ "n", "x", "o" }, "H", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], { expr = true })
Map({ "n", "x", "o" }, "L", "$")

Map("n", "J", "gi")
Map({ "n", "x" }, "U", "<c-r>")

Map("n", "K", "<nop>")
Map("n", "KK", "K")
Map("n", "KG", require("gitsigns").preview_hunk)
Map("n", "KA", function() require("gitsigns").blame_line({ full = true }) end)
Map("n", "KE", function() vim.diagnostic.open_float(0, { border = Border, scope = "line", source = "always" }) end)
Map("n", "KT", function() require("neotest").output.open() end)
Map("n", "KD", function() require("dap.ui.widgets").hover() end, { silent = true })

Map("n", "Q", "@q")
Map("x", "Q", ":norm! @q<cr>")

Map("n", "S", "<cmd>ISwapWith<cr>")

Map({ "n", "x", "o" }, "s", require("hop").hint_char1)

Map({ "n", "x", "o" }, "'", "`")
Map({ "n", "x", "o" }, "`", "'")

Map("x", "<", "<gv")
Map("x", ">", ">gv")

Map("n", "<c-v>", "<cmd>silent vsplit<cr>")
Map("n", "<c-x>", "<cmd>silent split<cr>")

-- GOTO
Map({ "n", "x", "o" }, "gj", "G")
Map({ "n", "x", "o" }, "gk", "gg")
Map({ "n", "x", "o" }, "gh", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], { expr = true })
Map({ "n", "x", "o" }, "gl", "$")

Map({ "n", "x", "o" }, "gt", "H")
Map({ "n", "x", "o" }, "gm", "M")
Map({ "n", "x", "o" }, "gb", "L")

Map("n", "gF", ":edit <cfile><cr>")
Map("n", "gx", ":!xdg-open <cfile> &<cr><cr>")

-- VIEW
Hydra({
    name = "View",
    mode = { "n", "x" },
    body = "v",
    config = {
        hint = {
            position = "middle-right",
            border = Border
        }
    },
    hint = [[
 _h_/_j_/_k_/_l_: ←/↓/↑/→
 _H_/_J_/_K_/_L_: ⇚/⟱/⤊/⇛
       _t_: top
       _v_: middle
       _b_: bottom
       _s_: start
       _m_: middle
       _e_: end
]]   ,
    heads = {
        { "h", "zh" },
        { "J", "<c-d>" },
        { "K", "<c-u>" },
        { "L", "zL" },

        { "H", "zH" },
        { "j", "<c-e>" },
        { "k", "<c-y>" },
        { "l", "zl" },

        { "t", "zt", { exit = true } },
        { "v", "zz", { exit = true } },
        { "b", "zb", { exit = true } },

        { "s", "zs", { exit = true } },
        { "m", "<cmd>set sidescrolloff=999<cr>hl<cmd>set sidescrolloff=0<cr>", { exit = true } },
        { "e", "ze", { exit = true } },
    }
})
Map({ "n", "x" }, "vt", "zt")
Map({ "n", "x" }, "vv", "zz")
Map({ "n", "x" }, "vb", "zb")
Map({ "n", "x" }, "vs", "zs")
Map({ "n", "x" }, "vm", "<cmd>set sidescrolloff=999<cr>hl<cmd>set sidescrolloff=0<cr>")
Map({ "n", "x" }, "ve", "ze")

Map({ "n", "x" }, "m", "v")

Map("n", ",j", "m1J`1")
Map("n", ",k", require("trevj").format_at_cursor)
Map("x", ",j", "J")

Map({ "n", "x" }, "R", "<plug>(SubversiveSubstitute)")

Map("n", ",rr", vim.lsp.buf.rename)

Map("n", ",rf", function() require("refactoring").refactor("Extract Block") end)
Map("x", ",rf", function() require("refactoring").refactor("Extract Function") end)
Map("n", ",rF", function() require("refactoring").refactor("Extract Block to File") end)
Map("x", ",rF", function() require("refactoring").refactor("Extract Function to File") end)
Map("n", ",re", "mia:lua require('refactoring').refactor('Extract Variable')<cr>", { remap = true })
Map("x", ",re", function() require("refactoring").refactor("Extract Variable") end)
Map({ "n", "x" }, ",ri", function() require("refactoring").refactor("Inline Variable") end)

Map("n", ",rd", "<cmd>Neogen<cr>")

Map("n", ",dd", function() require("refactoring").debug.printf({}) end)
Map({ "n", "x" }, ",dv", function() require("refactoring").debug.print_var({}) end, { remap = false })
Map("n", ",dq", function() require("refactoring").debug.cleanup({}) end)

Map({ "n", "x" }, ",s", "<Plug>Opsort", { remap = true })
Map("n", ",ss", "<Plug>OpsortLines", { remap = true })

-- Map({ "n", "x" }, ",t", "<Plug>(EasyAlign)")

vim.g.UnconditionalPaste_no_mappings = true
Map("n", ",Pb", "<Plug>UnconditionalPasteBlockBefore", { remap = true })
Map("n", ",PB", "<Plug>UnconditionalPasteJaggedBefore", { remap = true })
Map("n", ",Pi", "<Plug>UnconditionalPasteInlinedBefore", { remap = true })
Map("n", ",Pl", "<plug>UnconditionalPasteLineBefore", { remap = true })
Map("n", ",PS", "<Plug>UnconditionalPasteParagraphedBefore", { remap = true })
Map("n", ",Ps", "<Plug>UnconditionalPasteSpacedBefore", { remap = true })

Map("n", ",pb", "<Plug>UnconditionalPasteBlockAfter", { remap = true })
Map("n", ",pB", "<Plug>UnconditionalPasteJaggedAfter", { remap = true })
Map("n", ",pi", "<Plug>UnconditionalPasteInlinedAfter", { remap = true })
Map("n", ",pl", "<plug>UnconditionalPasteLineAfter", { remap = true })
Map("n", ",pS", "<Plug>UnconditionalPasteParagraphedAfter", { remap = true })
Map("n", ",ps", "<Plug>UnconditionalPasteSpacedAfter", { remap = true })

Map({ "n", "x" }, ",ff", vim.lsp.buf.format)
Map("n", ",fw", function()
    return "m1!ippar w" .. (vim.b.textwidth or vim.g.textwidth) .. "<cr>`1"
end, { expr = true, silent = true })
Map("n", ",fW", [["<cmd>%!par w" . &textwidth . "<cr>"]], { expr = true })
Map("x", ",fw", [["!par w" . &textwidth . "<cr>"]], { expr = true })
vim.g.caser_no_mappings = true
Map("n", ",fp", "<Plug>CaserMixedCase", { remap = true })
Map("n", ",fc", "<Plug>CaserCamelCase", { remap = true })
Map("n", ",fs", "<Plug>CaserSnakeCase", { remap = true })
Map("n", ",fu", "<Plug>CaserUpperCase", { remap = true })
Map("n", ",ft", "<Plug>CaserTitleCase", { remap = true })
Map("n", ",fd", "<Plug>CaserSentenceCase", { remap = true })
Map("n", ",f<space>", "<Plug>CaserSpaceCase", { remap = true })
Map("n", ",fk", "<Plug>CaserKebabCase", { remap = true })
Map("n", ",fk", "<Plug>CaserTitleKebabCase", { remap = true })
Map("n", ",f.", "<Plug>CaserDotCase", { remap = true })
Map("x", ",fp", "<Plug>CaserVMixedCase", { remap = true })
Map("x", ",fc", "<Plug>CaserVCamelCase", { remap = true })
Map("x", ",f_", "<Plug>CaserVSnakeCase", { remap = true })
Map("x", ",fu", "<Plug>CaserVUpperCase", { remap = true })
Map("x", ",ft", "<Plug>CaserVTitleCase", { remap = true })
Map("x", ",fs", "<Plug>CaserVSentenceCase", { remap = true })
Map("x", ",f<space>", "<Plug>CaserVSpaceCase", { remap = true })
Map("x", ",f-", "<Plug>CaserVKebabCase", { remap = true })
Map("x", ",fk", "<Plug>CaserVTitleKebabCase", { remap = true })
Map("x", ",f.", "<Plug>CaserVDotCase", { remap = true })

Map("n", "<leader><leader>", "<cmd>silent e #<cr>")

Map("n", "<leader>c", require("harpoon.mark").add_file)
Map("n", "<leader>v", require("harpoon.ui").toggle_quick_menu)
local harpoon_keys = { "a", "r", "s", "t" }
for i, key in pairs(harpoon_keys) do
    Map("n", "<leader>" .. key, function() require("harpoon.ui").nav_file(i) end)
end

Map("n", "<leader>h", "<cmd>OverseerTaskAction<cr>")
Map("n", "<leader>n", "<cmd>OverseerToggle<cr>")
Map("n", "<leader>e", "<cmd>OverseerRun<cr>")
-- Map("n", "<leader>E", function()
--     local overseer = require("overseer")
--     local tasks = overseer.list_tasks({ recent_first = true })
--     if vim.tbl_isempty(tasks) then
--         vim.notify("No tasks found", vim.log.levels.WARN)
--     else
--         overseer.run_action(tasks[1], "restart")
--     end
-- end)
Map("n", "<leader>i", "<cmd>OverseerQuickAction toggle<cr>")
Map("n", "<leader>o", function() require("neotest").summary.toggle() end)

Map("n", "<leader>l", function() require("neotest").run.run() end)


Map("n", "<leader>//", "<cmd>silent A<cr>")
Map("n", "<leader>/r", "<cmd>silent Ereadme<cr>")
Map("n", "<leader>/d", "<cmd>silent Edoc<cr>")
Map("n", "<leader>/D", "<cmd>silent EmainDoc<cr>")
Map("n", "<leader>/s", "<cmd>silent Esource<cr>")
Map("n", "<leader>/S", "<cmd>silent EmainSource<cr>")
Map("n", "<leader>/t", "<cmd>silent Etest<cr>")
Map("n", "<leader>/T", "<cmd>silent EmainTest<cr>")
Map("n", "<leader>/p", "<cmd>silent Edeps<cr>")
Map("n", "<leader>/b", "<cmd>silent Ebench<cr>")
Map("n", "<leader>/B", "<cmd>silent EmainBench<cr>")

Map("n", "<leader>f", function() ProjectFiles() end)
Map("n", "<leader>F", "<cmd>Telescope resume<cr>")

Map("n", ",gr", "<cmd>Gitsigns reset_hunk<CR>")
Map("n", ",gs", "<cmd>Gitsigns stage_hunk<CR>")

-- Insert Bindings

vim.cmd([[inoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[inoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[snoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[snoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])

Map({ "i", "s", "c" }, "<c-a>", "<HOME>")
Map({ "i", "s", "c" }, "<c-e>", "<END>")

Map({ "i", "s" }, "<c-]>", "<plug>luasnip-next-choice")
Map({ "i", "s", "c" }, "<c-space>", function() _G.cmp_toggle() end)

Map("n", "<c-leftmouse>", "<cmd>Telescope lsp_definitions theme=get_ivy<cr>")
Map("n", "<c-rightmouse>", "gf")

Ls = require("luasnip")
Map({ "i", "s" }, "<tab>", function()
    if Ls.expand_or_jumpable() then
        Ls.expand_or_jump()
    else
        vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<tab>", true, true, true), "n", false)
    end
end, { silent = true })

Map({ "i", "s" }, "<s-tab>", function()
    if Ls.jumpable(-1) then
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
