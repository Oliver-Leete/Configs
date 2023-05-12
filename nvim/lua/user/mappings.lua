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

Map({ "n", "x", "o" }, "Y", "<nop>")
Map({ "n", "x", "o" }, "C", "<nop>")
Map({ "n", "x", "o" }, "D", "<nop>")
Map({ "n", "x", "o" }, "S", "<nop>")
Map({ "n", "x", "o" }, "G", "<nop>")
Map({ "n", "x", "o" }, "K", "<nop>")
Map({ "n", "x" }, "H", "<nop>")


Map({ "n", "x", "o" }, "$", "<nop>")
Map({ "n", "x", "o" }, "^", "<nop>")

Map({ "n", "x", "o" }, "(", "<nop>")
Map({ "n", "x", "o" }, ")", "<nop>, test")
-- NOTE: D, Y, H, L, £, _, =, |, ;, ^, <BS>, <CR> are free to map
-- NOTE: H and L are taken in op mode
-- NOTE: y, d, c are free in op mode

-- Mappings
Map("n", "<esc>", function()
    vim.cmd("Noice dismiss")
    require("substitute.exchange").cancel()
end)

Map({ "n", "x", "o" }, "<m-f>", ";")
Map({ "n", "x", "o" }, "<m-F>", ",")
Map({ "n", "x", "o" }, "<m-t>", ";")
Map({ "n", "x", "o" }, "<m-T>", ",")

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

Map("n", "R", require("substitute").operator)
Map("x", "R", require("substitute").visual)
Map("n", "$", require("substitute.exchange").operator)
Map("x", "$", require("substitute.exchange").visual)

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

local help_hint = [[
┏^^^^━━━━━┳━━━━━━┳━━━━━^^^^┓
┃^^^^     ┃ Help ┃     ^^^^┃
┃^^^^     ┗━━━━━━┛     ^^^^┃
┃^^^^      Inline      ^^^^┃
┣^^^^━━━━━━━━━━━━━━━━━━^^^^┫
┃^^    _E_: errors     ^^^^┃
┃^^    _G_: git diff   ^^^^┃
┃^^^^                  ^^^^┃
┃^^^^      Pop-up      ^^^^┃
┣^^^^━━━━━━━━━━━━━━━━━━^^^^┫
┃^^    _K_: info       ^^^^┃
┃^^    _g_: git diff   ^^^^┃
┃^^    _b_: gid blame  ^^^^┃
┃^^    _e_: errors     ^^^^┃
┃^^    _t_: test out   ^^^^┃
┃^^    _d_: debug out  ^^^^┃
┣^^^^━━━━━━━━━━━━━━━━━━^^^^┫
┃^^    _<esc>_: exit   ^^^^┃
┗^^^^━━━━━━━━━━━━━━━━━━^^^^┛
]]
-- VIEW
Hydra({
    name = "Help",
    mode = { "n", "x" },
    body = "K",
    config = {
        color = "blue",
        invoke_on_body = true,
        hint = {
            position = "top-right",
            border = nil
        }
    },
    hint = help_hint,
    heads = {
        { "K",     vim.lsp.buf.hover },
        { "G",     require("gitsigns").preview_hunk_inline },
        { "E",     require("user.lsp").preview_diagnostics_inline },
        { "g",     require("gitsigns").preview_hunk },
        { "b",     function() require("gitsigns").blame_line({ full = true }) end },
        { "e",     function() vim.diagnostic.open_float({ border = Border, scope = "line", source = "always" }) end },
        { "t",     function() require("neotest").output.open() end },
        { "d",     function() require("dap.ui.widgets").hover() end,                                                { silent = true } },
        { "<esc>", nil,                                                                                             { nowait = true } },
    }
})

Map("n", "Q", "@q")
Map("x", "Q", ":norm! @q<cr>")

Map({ "n", "x", "o" }, "s", require("hop").hint_char1)

Map("x", "<", "<gv")
Map("x", ">", ">gv")

Map("x", "/", "<esc>/\\%V")

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

Map({ "n", "x", "o" }, "gV", "`[v`]")
Map("n", "gF", ":edit <cfile><cr>")
Map("n", "gx", ":!xdg-open <cfile> &<cr><cr>")

-- VIEW
local view_hint = [[
┏^^^^━━━━━┳━━━━━━┳━━━━━^^^^┓
┃^^^^     ┃ View ┃     ^^^^┃
┃^^^^     ┗━━━━━━┛     ^^^^┃
┃^^^^       Move       ^^^^┃
┣^^^^━━━━━━━━━━━━━━━━━━^^^^┫
┃ _h_/_j_/_k_/_l_: ←/↓/↑/→ ┃
┃ _H_/_J_/_K_/_L_: ⇚/⟱/⤊/⇛ ┃
┃^^^^                  ^^^^┃
┃^^^^       Jump       ^^^^┃
┣^^^^━━━━━━━━━━━━━━━━━━^^^^┫
┃^^    _t_: top        ^^^^┃
┃^^    _v_: middle     ^^^^┃
┃^^    _b_: bottom     ^^^^┃
┃^^    _s_: start      ^^^^┃
┃^^    _m_: middle     ^^^^┃
┃^^    _e_: end        ^^^^┃
┣^^^^━━━━━━━━━━━━━━━━━━^^^^┫
┃^^    _<esc>_: exit   ^^^^┃
┗^^^^━━━━━━━━━━━━━━━━━━^^^^┛
]]

local view_heads = {
    { "H",     "zH" },
    { "J",     "<c-d>" },
    { "K",     "<c-u>" },
    { "L",     "zL" },

    { "h",     "zh" },
    { "j",     "<c-e>" },
    { "k",     "<c-y>" },
    { "l",     "zl" },

    { "t",     "zt" },
    { "v",     "zz" },
    { "b",     "zb" },

    { "s",     "zs" },
    { "m",     "<cmd>set sidescrolloff=999<cr>hl<cmd>set sidescrolloff=0<cr>" },
    { "e",     "ze" },
    { "<esc>", nil,                                                           { exit = true, nowait = true, desc = "exit" } },
}

Hydra({
    name = "View",
    mode = { "n", "x" },
    body = "v",
    config = {
        color = "teal",
        invoke_on_body = true,
        hint = {
            position = "top-right",
            border = nil
        }
    },
    hint = view_hint,
    heads = view_heads,
})

Hydra({
    name = "View",
    mode = { "n", "x" },
    body = "V",
    config = {
        color = "amaranth",
        invoke_on_body = true,
        hint = {
            position = "top-right",
            border = nil
        }
    },
    hint = view_hint,
    heads = vim.list_extend({ { "V", nil, { exit = true, nowait = true, desc = false } } }, view_heads)
})

-- Map({ "n", "x" }, "m", "v")

local mini_move_hint = [[
┏^^^^━━━━━┳━━━━━━┳━━━━━^^^^┓
┃^^^^     ┃ Tree ┃     ^^^^┃
┃^^^^     ┗━━━━━━┛     ^^^^┃
┃^^^^       Move       ^^^^┃
┣^^^^━━━━━━━━━━━━━━━━━━^^^^┫
┃ _H_/_J_/_K_/_L_: ⇚/⟱/⤊/⇛ ┃
┃^^^^                  ^^^^┃
┣^^^^━━━━━━━━━━━━━━━━━━^^^^┫
┃^^    _<esc>_: exit   ^^^^┃
┗^^^^━━━━━━━━━━━━━━━━━━^^^^┛
]]
local old_virt = vim.wo.virtualedit
require('hydra')({
    name = "Move",
    mode = { "x" },
    body = "M",
    config = {
        color = "pink",
        invoke_on_body = true,
        hint = {
            position = "top-right",
            border = nil
        },
        on_enter = function()
            old_virt = vim.wo.virtualedit
            vim.wo.virtualedit = "all"
        end,
        on_exit = function()
            vim.wo.virtualedit = old_virt
        end,
    },
    hint = mini_move_hint,
    heads = {
        { 'H', function() require('mini.move').move_selection("left") end,  { nowait = true } },
        { 'J', function() require('mini.move').move_selection("down") end,  { nowait = true } },
        { 'K', function() require('mini.move').move_selection("up") end,    { nowait = true } },
        { 'L', function() require('mini.move').move_selection("right") end, { nowait = true } },
        { 'M', nil, {
            exit = true,
            nowait = true,
            desc = false
        } },
        { '<esc>', nil, { exit = true, nowait = true } },
    }
})
require('hydra')({
    name = "Move Line",
    mode = { "n" },
    body = "M",
    config = {
        color = "pink",
        invoke_on_body = true,
        on_enter = function()
            old_virt = vim.wo.virtualedit
            vim.wo.virtualedit = "all"
        end,
        on_exit = function()
            vim.wo.virtualedit = old_virt
        end,
        hint = {
            position = "top-right",
            border = nil
        }
    },
    hint = mini_move_hint,
    heads = {
        { 'H',     function() require('mini.move').move_line("left") end,  { nowait = true } },
        { 'J',     function() require('mini.move').move_line("down") end,  { nowait = true } },
        { 'K',     function() require('mini.move').move_line("up") end,    { nowait = true } },
        { 'L',     function() require('mini.move').move_line("right") end, { nowait = true } },
        { 'M',     nil,                                                    { exit = true, nowait = true, desc = false } },
        { '<esc>', nil,                                                    { exit = true, nowait = true } },
    }
})
-- Text leader mappings: ,

Map("n", ",rr", vim.lsp.buf.rename)

Map({ "n", "x" }, ",rt", require("ssr").open)
Map({ "n", "x" }, ",rs", ":MurenFresh<cr>")
Map({ "n", "x" }, ",rS", ":MurenOpen<cr>")
Map({ "n", "x" }, ",r/", ":MurenUnique<cr>")

Map("n", ",rf", function() require("refactoring").refactor("Extract Block") end)
Map("x", ",rf", function() require("refactoring").refactor("Extract Function") end)
Map("x", ",rF", require("genghis").moveSelectionToNewFile)
Map("n", ",re", "mia:lua require('refactoring').refactor('Extract Variable')<cr>", { remap = true })
Map("x", ",re", function() require("refactoring").refactor("Extract Variable") end)
Map({ "n", "x" }, ",ri", function() require("refactoring").refactor("Inline Variable") end)

Map("n", ",aa", function() require("neogen").generate({ type = "func" }) end)
Map("n", ",as", function() require("neogen").generate({ type = "class" }) end)
Map("n", ",at", function() require("neogen").generate({ type = "type" }) end)
Map("n", ",af", function() require("neogen").generate({ type = "file" }) end)

Map("n", ",dd", function() require("refactoring").debug.printf({}) end)
Map({ "n", "x" }, ",dv", function() require("refactoring").debug.print_var({}) end, { remap = false })
Map("n", ",dq", function() require("refactoring").debug.cleanup({}) end)

Map({ "n", "x" }, ",s", "<Plug>Opsort", { remap = true })
Map("n", ",ss", "<Plug>OpsortLines", { remap = true })

Map("n", ",n", require("ts-node-action").node_action)

Map({ "n", "x" }, ",pl", function() require("user.myfuncs").paste_special(vim.v.register, "l", "p") end)
Map({ "n", "x" }, ",Pl", function() require("user.myfuncs").paste_special(vim.v.register, "l", "P") end)
Map({ "n", "x" }, ",pi", function() require("user.myfuncs").paste_special(vim.v.register, "c", "p") end)
Map({ "n", "x" }, ",Pi", function() require("user.myfuncs").paste_special(vim.v.register, "c", "P") end)
Map({ "n", "x" }, ",pb", function() require("user.myfuncs").paste_special(vim.v.register, "b", "p") end)
Map({ "n", "x" }, ",Pb", function() require("user.myfuncs").paste_special(vim.v.register, "b", "P") end)

Map({ "n", "x" }, ",ff", function()
    pcall(Ls.unlink_current)
    vim.lsp.buf.format()
end)
Map({ "n", "x" }, ",fe", require("null-ls-embedded").format_current)
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

Map("n", "<leader>c", function()
    local bufname = vim.api.nvim_buf_get_name(0)
    require("harpoon.mark").toggle_file(bufname)
end)
Map("n", "<leader>v", require("harpoon.ui").toggle_quick_menu)
Map("n", "<leader>V", require("harpoon.mark").clear_all)
local harpoon_keys = { "a", "r", "s", "t" }
for i, key in pairs(harpoon_keys) do
    Map("n", "<leader>" .. key, function() require("harpoon.ui").nav_file(i) end)
end

local action_util = require("overseer.action_util")
local overseer = require("overseer")
Map("n", "<leader>h", function()
    local bufnr = vim.api.nvim_get_current_buf()
    local task = vim.tbl_filter(function(t) return (t.strategy.bufnr == bufnr) end, overseer.list_tasks())
        [1]
    if task then
        action_util.run_task_action(task)
    else
        vim.cmd("OverseerTaskAction")
    end
end)
Map("n", "<leader>H", "<cmd>OverseerTaskAction<cr>")
Map("n", "<leader>n", "<cmd>OverseerToggle bottom<cr>")
Map("n", "<leader>e", "<cmd>OverseerRun<cr>")
Map("n", "<leader>I", function()
    if SendID then
        vim.fn.chansend(SendID, vim.api.nvim_get_current_line() .. "\n")
    else
        vim.cmd.echomsg({ args = { "'No Term set as send term'" } })
    end
end)
Map("x", "<leader>I", function()
    if SendID then
        -- does not handle rectangular selection
        local s_start = vim.fn.getpos("'<")
        local s_end = vim.fn.getpos("'>")
        local n_lines = math.abs(s_end[2] - s_start[2]) + 1
        local lines = vim.api.nvim_buf_get_lines(0, s_start[2] - 1, s_end[2], false)
        lines[1] = string.sub(lines[1], s_start[3], -1)
        if n_lines == 1 then
            lines[n_lines] = string.sub(lines[n_lines], 1, s_end[3] - s_start[3] + 1)
        else
            lines[n_lines] = string.sub(lines[n_lines], 1, s_end[3])
        end
        local selection = table.concat(lines, '\n')
        vim.fn.chansend(SendID, selection .. "\n")
    else
        vim.cmd.echomsg({ args = { "'No Term set as send term'" } })
    end
end)
Map("n", "<leader>o", function() require("neotest").summary.toggle() end)

Map("n", "<leader>l", function() require("neotest").run.run() end)
Map("n", "<leader>L", function() require("neotest").run.run_last() end)


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

Map("n", "<leader>w",
    function() require("telescope.builtin").lsp_workspace_symbols(require("telescope.themes").get_ivy()) end)
Map("n", "<leader>W", function() require("telescope.builtin").live_grep(require("telescope.themes").get_ivy()) end)
Map("n", "<leader>f", function() ProjectFiles() end)
Map("n", "<leader>F", "<cmd>Telescope resume<cr>")

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

Map({ "i", "s" }, "<c-]>", "<plug>luasnip-next-choice")
Map({ "i", "s", "c" }, "<c-space>", function() _G.cmp_toggle() end)

Ls = require("luasnip")
Map({ "i", "s" }, "<tab>", function()
    if Ls.expand_or_locally_jumpable() then
        Ls.expand_or_jump()
    else
        require("tabout").tabout()
    end
end, { silent = true })

Map({ "i", "s" }, "<s-tab>", function()
    if Ls.locally_jumpable(-1) then
        Ls.jump(-1)
    else
        require("tabout").taboutBack()
    end
end, { silent = true })

Map("i", "<c-n>", function()
    if Ls.choice_active() then
        Ls.change_choice(1)
    end
end)

-- Terminal Bindings

Map("t", "<c-]>", "<c-\\><c-n>")

local termMap = vim.api.nvim_create_augroup("termMap", {})
vim.api.nvim_create_autocmd(
    "BufEnter",
    {
        pattern = "*",
        group = termMap,
        callback = function()
            if vim.bo.buftype == "terminal" then
                Map("n", "<esc>", "<cmd>OverseerOpen bottom<cr>", { buffer = 0 })
            end
        end
    }

)
