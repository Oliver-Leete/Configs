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
Map("n", "<esc>", function()
    vim.cmd("Noice dismiss")
    require("edgy").close()
end)

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
Hydra({
    name = "Help",
    mode = { "n", "x" },
    body = "K",
    config = {
        color = "teal",
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

Map({ "n", "x", "o" }, "s", require("flash").jump)
Map({ "n", "o" }, "z", require("flash").treesitter)
Map({ "o" }, "S", require("flash").remote)

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
Map("n", "gx", function() vim.ui.open(vim.fn.expand("<cfile>")) end)

Map({ "n", "x" }, "gz", function()
    if vim.b.upafunc then
        vim.b.upafunc()
    else
        vim.notify("No up function found for this filetype", vim.log.levels.WARN)
    end
end)

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
┃^^    _o_: only window^^^^┃
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

    { "o",     "<c-w>o" },
    { "<esc>", nil,                                                           { exit = true, nowait = true } },
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
        { 'J', function() require('mini.move').move_selection("down") end,  { nowait = true } },
        { 'H', function() require('mini.move').move_selection("left") end,  { nowait = true } },
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
MoveLine = require('hydra')({
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
-- text leader mappings

Map("n", ",,", require("binary-swap").swap_operands)

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

Map("n", ",dd", function() require("refactoring").debug.printf({}) end)
Map({ "n", "x" }, ",dv", function() require("refactoring").debug.print_var({}) end, { remap = false })
Map("n", ",dq", function() require("refactoring").debug.cleanup({}) end)

Map({ "n", "x" }, ",s", "<Plug>Opsort", { remap = true })

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
Map("n", ",fw", function()
    local num = (vim.b.textwidth and vim.b.textwidth > 0) and vim.b.textwidth or vim.g.textwidth
    return "m1!ippar w" .. num .. "<cr>`1"
end, { expr = true, silent = true })
Map("n", ",fW", [["<cmd>%!par w" . &textwidth . "<cr>"]], { expr = true })
Map("x", ",fw", [["!par w" . &textwidth . "<cr>"]], { expr = true })

Map("n", ",fp", function() require('textcase').operator('to_pascal_case') end)
Map("n", ",fc", function() require('textcase').operator('to_camel_case') end)
Map("n", ",fs", function() require('textcase').operator('to_snake_case') end)
Map("n", ",fu", function() require('textcase').operator('to_upper_case') end)
Map("n", ",ft", function() require('textcase').operator('to_title_case') end)
Map("n", ",fa", function() require('textcase').operator('to_phrase_case') end)
Map("n", ",fl", function() require('textcase').operator('to_lower_case') end)
Map("n", ",fk", function() require('textcase').operator('to_dash_case') end)
Map("n", ",f:", function() require('textcase').operator('to_constant_case') end)
Map("n", ",f.", function() require('textcase').operator('to_dot_case') end)
Map("n", ",f/", function() require('textcase').operator('to_path_case') end)

Map("n", ",fP", function() require('textcase').lsp_rename('to_pascal_case') end)
Map("n", ",fC", function() require('textcase').lsp_rename('to_camel_case') end)
Map("n", ",fS", function() require('textcase').lsp_rename('to_snake_case') end)
Map("n", ",fU", function() require('textcase').lsp_rename('to_upper_case') end)
Map("n", ",fT", function() require('textcase').lsp_rename('to_title_case') end)
Map("n", ",fA", function() require('textcase').lsp_rename('to_phrase_case') end)
Map("n", ",fL", function() require('textcase').lsp_rename('to_lower_case') end)
Map("n", ",fK", function() require('textcase').lsp_rename('to_dash_case') end)
Map("n", ",f;", function() require('textcase').lsp_rename('to_constant_case') end)
Map("n", ",f>", function() require('textcase').lsp_rename('to_dot_case') end)
Map("n", ",f?", function() require('textcase').lsp_rename('to_path_case') end)

Map("n", "<leader><leader>", "<cmd>silent e #<cr>")

Map("n", "<leader>c", require("grapple").toggle)
Map("n", "<leader>v", require("grapple").popup_tags)
local grapple_keys = { "a", "r", "s", "t" }
for i, key in pairs(grapple_keys) do
    Map("n", "<leader>" .. key, function() require("grapple").select({ key = i }) end)
end

local action_util = require("overseer.action_util")
local overseer = require("overseer")

Map("n", "<leader>n", "<cmd>OverseerRun<cr>")
Map("n", "<leader>N", function()
    local tasks = overseer.list_tasks({ recent_first = true })
    if vim.tbl_isempty(tasks) then
        vim.notify("No tasks found", vim.log.levels.WARN)
    else
        overseer.run_action(tasks[1], "restart")
    end
end)
Map("n", "<leader>e", function()
    local bufnr = vim.api.nvim_get_current_buf()
    local task = vim.tbl_filter(function(t) return (t.strategy.bufnr == bufnr) end, overseer.list_tasks())[1]
    if task then
        action_util.run_task_action(task)
    else
        vim.cmd("OverseerTaskAction")
    end
end)
Map("n", "<leader>E", "<cmd>OverseerTaskAction<cr>")
Map("n", "<leader>i", function() require("neotest").run.run() end)
Map("n", "<leader>I", function() require("neotest").run.run_last() end)
Map("n", "<leader>o", function() require("neotest").run.run({ strategy = "dap" }) end)
Map("n", "<leader>O", function() require("neotest").run.run_last({ strategy = "dap" }) end)

Map("n", "<leader>h", "<cmd>OverseerToggle<cr>")
Map("n", "<leader>k", function()
    require("neotest").summary.toggle()
    local win = vim.fn.bufwinid("Neotest Summary")
    if win > -1 then
        vim.api.nvim_set_current_win(win)
    end
end)

Map("n", "<leader>m", function() require("trouble").toggle({ mode = "quickfix" }) end)
Map("n", "<leader>,", "<cmd>OverseerQuickAction open<cr>")
Map("n", "<leader><", "<cmd>OverseerQuickAction open here<cr>")
Map("n", "<leader>.", function()
    require("user.myfuncs").toggle_noice()
end)
Map("n", "<leader>/", function()
    vim.cmd.vsplit(); require("neotest").output_panel.toggle()
end)

Map("n", "<s-cr>", function()
    if SendID then
        vim.fn.chansend(SendID, vim.api.nvim_get_current_line() .. "\n")
    else
        vim.notify("No Term set as send term", vim.log.levels.WARN)
    end
end)

local send_code = function(start_mark, end_mark)
    local s_start = vim.fn.getpos(start_mark)
    local s_end = vim.fn.getpos(end_mark)
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
end

Map("x", "<s-cr>", function()
    if SendID then
        vim.fn.feedkeys(vim.api.nvim_replace_termcodes("<esc>", true, true, true))
        send_code("'<", "'>")
    else
        vim.notify("No Term set as send term", vim.log.levels.WARN)
    end
end)

Send_motion_internal = function(motion)
    if SendID then
        if motion == nil then
            vim.go.operatorfunc = [[v:lua.Send_motion_internal]]
            vim.api.nvim_feedkeys('g@', 'ni', false)
        end
        send_code("'[", "']")
    else
        vim.notify("No Term set as send term", vim.log.levels.WARN)
    end
end

Map("n", "<c-cr>", function()
    vim.go.operatorfunc = [[v:lua.Send_motion_internal]]
    vim.api.nvim_feedkeys('g@', 'ni', false)
end)

Map("n", "<leader>w",
    function() require("telescope.builtin").lsp_dynamic_workspace_symbols(require("telescope.themes").get_ivy()) end)
Map("n", "<leader>W", function() require("telescope.builtin").live_grep(require("telescope.themes").get_ivy()) end)
Map("n", "<leader>f", function() ProjectFiles() end)
Map("n", "<leader>F", "<cmd>Telescope resume<cr>")

local projects = require("user.projects")

Map("n", "<leader>b", projects.load_session)
Map("n", "<leader>B", projects.save_session)

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
local Hydra = require("hydra")
local gitsigns = require("gitsigns")

local on = false
local toggle_show = function()
    if on then
        on = false
        gitsigns.toggle_linehl(false)
        gitsigns.toggle_deleted(false)
        gitsigns.toggle_word_diff(false)
    else
        on = true
        gitsigns.toggle_linehl(true)
        gitsigns.toggle_deleted(true)
        gitsigns.toggle_word_diff(true)
    end
end

Old_dir_jump = "search"
local hint = [[
┏^━━━━━━━━┳━━━━━┳━━━━━━━^┓
┃^        ┃ GIT ┃       ^┃
┃^        ┗━━━━━┛       ^┃
┃^         Hunks        ^┃
┣^━━━━━━━━━━━━━━━━━━━━━━^┫
┃^ n: next hunk         ^┃
┃^ N: prev hunk         ^┃
┃^                      ^┃
┃ _,s_: stage hunk       ┃
┃ _,r_: reset hunk       ┃
┃ _,u_: undo last stage  ┃
┃ _,S_: stage buffer     ┃
┃^                      ^┃
┃^          View        ^┃
┣^━━━━━━━━━━━━━━━━━━━━━━^┫
┃ _,K_: blame line       ┃
┃ _,p_: preview hunk     ┃
┃ _,d_: show diff        ┃
┣^━━━━━━━━━━━━━━━━━━━━━━^┫
┃ _,f_: file finder      ┃
┃ _,<esc>_: exit         ┃
┗^━━━━━━━━━━━━━━━━━━━━━━^┛
]]
Hydra({
    name = "Git",
    hint = hint,
    config = {
        color = "pink",
        invoke_on_body = true,
        hint = {
            position = "top-right",
            border = nil
        },
        on_enter = function()
            Old_dir_jump = vim.g.dirJumps
            vim.g.dirJumps = "h"
        end,
        on_exit = function()
            if vim.g.dirJumps == "h" then
                vim.g.dirJumps = Old_dir_jump
            end
            on = false
            gitsigns.toggle_linehl(false)
            gitsigns.toggle_deleted(false)
            gitsigns.toggle_word_diff(false)
        end,
    },
    mode = { "n", "x" },
    body = "<leader>g",
    heads = {
        { ",s",        gitsigns.stage_hunk,                          { silent = true, desc = "stage hunk" } },
        { ",r",        gitsigns.reset_hunk,                          { silent = true, desc = "stage hunk" } },
        { ",u",        gitsigns.undo_stage_hunk,                     { desc = "undo last stage" } },
        { ",S",        gitsigns.stage_buffer,                        { desc = "stage buffer" } },
        { ",p",        gitsigns.preview_hunk,                        { desc = "preview hunk" } },
        { ",d",        toggle_show,                                  { nowait = true, desc = "toggle diff" } },
        { ",K",        gitsigns.blame_line,                          { desc = "blame" } },
        { ",f",        "<cmd>Telescope git_status theme=get_ivy<cr>" },
        { ",<esc>",    nil,                                          { exit = true, nowait = true, desc = "exit" } },
        { "<leader>g", nil,                                          { exit = true, nowait = true, desc = false } },
    }
})


-- treesitter

local tc_settings = {
    highlight = true,
    higroup = "Search",
}
local tc = require('tree-climber')
local no_exit = false

local tc_ex = function()
    tc.select_node(tc_settings)
    require("mini.operators").exchange("visual")
end

local tc_mul = function()
    tc.select_node(tc_settings)
    require("mini.operators").multipy("visual")
end

local tc_goto_first = function()
    repeat
        local old_pos = vim.api.nvim_win_get_cursor(0)
        tc.goto_prev(tc_settings)
        local new_pos = vim.api.nvim_win_get_cursor(0)
    until old_pos[1] == new_pos[1] and old_pos[2] == new_pos[2]
end

local tc_goto_last = function()
    repeat
        local old_pos = vim.api.nvim_win_get_cursor(0)
        tc.goto_next(tc_settings)
        local new_pos = vim.api.nvim_win_get_cursor(0)
    until old_pos[1] == new_pos[1] and old_pos[2] == new_pos[2]
end

local ts_surf_hint = [[
┏^^^^━━━━━┳━━━━━━┳━━━━━^^^^┓
┃^^^^     ┃ Tree ┃     ^^^^┃
┃^^^^     ┗━━━━━━┛     ^^^^┃
┃^^^^       Move       ^^^^┃
┣^^^^━━━━━━━━━━━━━━━━━━^^^^┫
┃ _h_/_j_/_k_/_l_: ←/↓/↑/→ ┃
┃^^     _[_/_]_: ⇚/⇛     ^^┃
┃^^^^                  ^^^^┃
┃^^   _H_/_L_: swap      ^^┃
┃^^^^                  ^^^^┃
┃^^   _R_: raise       ^^^^┃
┃^^   _m_: select node ^^^^┃
┣^^^^━━━━━━━━━━━━━━━━━━^^^^┫
┃^^    _<esc>_: exit   ^^^^┃
┗^^^^━━━━━━━━━━━━━━━━━━^^^^┛
]]
TreeHydra = require('hydra')({
    name = "Treesitter",
    mode = { "n", "x" },
    body = "Z",
    config = {
        color = "red",
        invoke_on_body = true,
        hint = {
            position = "top-right",
            border = nil
        },
        on_enter = function() tc.highlight_node(tc_settings) end,
        on_exit = function()
            if no_exit == true then
                no_exit = false
            else
                tc.select_node(tc_settings)
            end
        end,
    },
    hint = ts_surf_hint,
    heads = {
        { '[',     tc_goto_first,                              { nowait = true } },
        { ']',     tc_goto_last,                               { nowait = true } },
        { 'h',     function() tc.goto_prev(tc_settings) end,   { nowait = true } },
        { 'j',     function() tc.goto_child(tc_settings) end,  { nowait = true } },
        { 'k',     function() tc.goto_parent(tc_settings) end, { nowait = true } },
        { 'l',     function() tc.goto_next(tc_settings) end,   { nowait = true } },
        { 'H',     function() tc.swap_prev(tc_settings) end,   { nowait = true } },
        { 'L',     function() tc.swap_next(tc_settings) end,   { nowait = true } },
        { 'R',     function() tc.raise(tc_settings) end,       { nowait = true } },
        { '$',     tc_ex,                                      { nowait = true, desc = false } },
        { '+',     tc_mul,                                     { nowait = true, desc = false } },
        { 'm',     function() tc.select_node(tc_settings) end, { exit = true } },
        { 'Z',     function() no_exit = true end,              { exit = true, nowait = true, desc = false } },
        { '<esc>', function() no_exit = true end,              { exit = true, nowait = true } },
    }
})

-- debuggin

local dapui = require("dapui")
local dap = require("dap")

local hint = [[
┏^^^━━━━━━━━┳━━━━━━┳━━━━━━━━^^^┓
┃^^^        ┃  DAP ┃        ^^^┃
┃^^^        ┗━━━━━━┛        ^^^┃
┃^^^          Step          ^^^┃
┣^^^━━━━━━━━━━━━━━━━━━━━━━━━^^^┫
┃^^^                        ^^^┃
┃^^^          back          ^^^┃
┃^^           _,k_           ^^┃
┃     out _,h_ ^^ _,l_ into    ┃
┃^^           _,j_           ^^┃
┃^^^          over          ^^^┃
┃^  ns-out _,<_  _,>_ ns-in   ^┃
┃^^^                        ^^^┃
┃^^^          Jump          ^^^┃
┣^^^━━━━━━━━━━━━━━━━━━━━━━━━^^^┫
┃ _,t_: toggle breakpoint  ^^^^┃
┃ _,T_: special breakpoint ^^^^┃
┃ _,r_: continue           ^^^^┃
┃ _,R_: continue to cursor ^^^^┃
┃ _,p_: pause              ^^^^┃
┃ _,X_: terminate          ^^^^┃
┣^^^━━━━━━━━━━━━━━━━━━━━━━━━^^^┫
┃ _<leader>j_: scopes      ^^^^┃
┃ _<leader>h_: breakpoints ^^^^┃
┃ _<leader>k_: stacks      ^^^^┃
┃ _<leader>m_: watches     ^^^^┃
┃ _<leader>,_: repl        ^^^^┃
┃ _<leader>._: consol      ^^^^┃
┣^^━━━━━━━━━━━━━━━━━━━━━━━━^^^^┫
┃ _,f_: breakpoint finder  ^^^^┃
┃ _,<esc>_: exit           ^^^^┃
┗^^━━━━━━━━━━━━━━━━━━━━━━━━^^^^┛
]]
DapHydra = require("hydra")({
    name = "Debug",
    hint = hint,
    config = {
        color = "pink",
        invoke_on_body = true,
        hint = {
            border = nil,
            position = "top-right"
        },
    },
    mode = { "n" },
    body = "<leader>d",
    heads = {
        { ",h", dap.step_out, { desc = "step out" } },
        { ",j", dap.step_over, { desc = "step over" } },
        { ",k", dap.step_back, { desc = "step back" } },
        { ",l", dap.step_into, { desc = "step into" } },
        { ",t", dap.toggle_breakpoint, { desc = "toggle breakpoint" } },
        { ",T", function()
            local cond = vim.fn.input("Breakpoint condition: ")
            local hit = vim.fn.input("Hit condition: ")
            local log = vim.fn.input("Log message: ")
            dap.set_breakpoint(cond, hit, log)
        end },
        { ",<", dap.up },
        { ",>", dap.down },
        { ",r", dap.continue, { desc = "continue" } },
        { ",R", dap.run_to_cursor },
        { ",X", dap.terminate, { desc = "terminate" } },
        { ",p", dap.pause },
        { "<leader>j", function() dapui.toggle({layout=6}) end },
        { "<leader>h", function() dapui.toggle({layout=4}) end },
        { "<leader>k", function() dapui.toggle({layout=5}) end },
        { "<leader>m", function() dapui.toggle({layout=3}) end },
        { "<leader>,", function() dapui.toggle({layout=2}) end },
        { "<leader>.", function() dapui.toggle({layout=1}) end },
        { ",<esc>", nil, { exit = true, nowait = true, desc = "exit" } },
        { ",f", "<cmd>Telescope dap list_breakpoints theme=get_ivy<cr>" },
        { "<leader>d", nil, { exit = true, nowait = true, desc = false } },
    }
})
