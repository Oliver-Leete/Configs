mapxName = require("mapx").setup({ global = true })
local silent = mapxName.silent
Map = vim.keymap.set

-- Leader Mapping
vim.opt.timeoutlen = 500
vim.api.nvim_set_var("mapleader", " ")
vim.api.nvim_set_var("maplocalleader", "\\")

Map({ "n", "x", "o" }, "<BackSPACE>", "<Nop>")
Map({ "n", "x", "o" }, "<SPACE>", "<Nop>")

Map({ "n", "x", "o" }, ",", "<Nop>")
Map({ "n", "x", "o" }, ";", "<Nop>")

-- Un-Mappings
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

-- NOTE: _, =, |, :, ^, BS are free to map

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
Map("x", "<M-;>", "o")

Map({ "n", "x" }, "<m-c>", [["_c]])
Map({ "n", "x" }, "<m-d>", [["_d]])

Map("n", "<m-o>", "m1o<esc>`1")
Map("n", "<m-O>", "m1O<esc>`1")
Map("n", "<m-o>", "<esc>`>o<esc>gv")
Map("n", "<m-O>", "<esc>`<O<esc>gv")

Map("x", "I", "I")
Map("x", "A", "A")

Map("n", "<leader>n", require("harpoon.mark").add_file)
Map("n", "<leader>e", require("harpoon.ui").toggle_quick_menu)
local harpoon_keys = { "a", "r", "s", "t" }
for i, key in pairs(harpoon_keys) do
    Map("n", "<leader>" .. key, function()
        require("harpoon.ui").nav_file(i)
    end)
end

Map("n", "<c-/>", ",cc", { remap = true })
Map("x", "<c-/>", ",c", { remap = true })

Map("n", "£", [[:exe "let @/='" . expand("<cWORD>") . "' "<cr>]], { silent = true })

vim.cmd([[inoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[inoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[snoremap <expr> <nowait> <c-y> matchstr(getline(line('.')-1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])
vim.cmd([[snoremap <expr> <nowait> <c-l> matchstr(getline(line('.')+1),'\%'.virtcol('.').'v\%(\k\+\\|.\)')]])

Map("i", "<c-g>", "<c-o>%")
Map("i", "<c-s>", require("lsp_signature").toggle_float_win)

Map("i", ",", ",<c-g>u")
Map("i", ".", ".<c-g>u")
Map("i", "!", "!<c-g>u")
Map("i", "?", "?<c-g>u")

-- Panel Specific Mappings
local panelMappings = vim.api.nvim_create_augroup("panelMappings", { clear = true })
vim.api.nvim_create_autocmd("filetype", {
    pattern = { "qf", "help", "vim-plug", "juliadoc", "lspinfo", "tsplayground", "harpoon-menu" },
    callback = function()
        Map("n", "<esc>", "<cmd>q<cr>", { buffer = 0 })
    end,
    group = panelMappings,
})
vim.api.nvim_create_autocmd("filetype", {
    pattern = { "DiffviewFiles", "DiffviewFileHistory" },
    callback = function()
        Map("n", "<esc>", "<cmd>DiffviewClose<cr>", { buffer = 0 })
    end,
    group = panelMappings,
})
vim.api.nvim_create_autocmd("filetype", {
    pattern = "undotree",
    callback = function()
        Map("n", "<esc>", "<cmd>UndotreeHide<cr>", { buffer = 0 })
    end,
    group = panelMappings,
})

vim.api.nvim_set_var("wordmotion_prefix", "$")

-- UnMap Plugins
vim.api.nvim_set_var("kitty_navigator_no_mappings", true)
vim.api.nvim_set_var("UnconditionalPaste_no_mappings", true)
vim.api.nvim_set_var("caser_no_mappings	=", true)
vim.api.nvim_set_var("textobj_markdown_no_default_key_mappings", true)
vim.api.nvim_set_var("julia_blocks", false)
vim.api.nvim_set_var("surround_no_mappings", true)
vim.api.nvim_set_var("wordmotion_nomap", true)

Map({ "n", "x", "o" }, "H", [[v:count?(v:count>5?"m'".v:count:'').'j':'gj']], { expr = true })
Map({ "n", "x", "o" }, "L", [[v:count?(v:count>5?"m'".v:count:'').'k':'gk']], { expr = true })

Map({ "n", "x", "o" }, "H", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], { expr = true })
Map({ "n", "x", "o" }, "L", [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], { expr = true })

Map("n", "J", "gi")
Map({ "n", "x" }, "U", "<c-r>")

Map("n", "K", "<nop>")
Map("n", "KK", "K")
Map("n", "KG", require("gitsigns").preview_hunk)
Map("n", "KA", function()
    require("gitsigns").blame_line({ full = true })
end)
Map("n", "KE", function()
    vim.diagnostic.open_float(0, { border = "single", scope = "line", source = "always" })
end)

Map("x", "J", ":move '>+1<cr>gv=gv")
Map("x", "K", ":move '<-2<cr>gv=gv")

Map("n", "Q", "@q")
Map("x", "Q", ":norm! @q<cr>")

Map("n", "s", require("hop").hint_char1)
Map("n", "S", "<cmd>ISwapWith<cr>")
Map({ "x", "o" }, "s", function()
    require("hop").hint_char1({ inclusive_jump = true })
end)

Map({ "n", "x", "o" }, "'", "`")
Map({ "n", "x", "o" }, "`", "'")

Map("x", "<", "<gv")
Map("x", ">", ">gv")

Map("n", "<c-v>", "<cmd>vsplit %<cr>")
Map("n", "<c-x>", "<cmd>split %<cr>")

Map("n", "<cr><cr>", "<cmd>call v:lua.sendLines(v:count)<cr>", { silent = true })
Map("n", "<cr>", "<plug>(sendOp)", { silent = true })
Map("x", "<cr>", "<plug>(sendReg)", { silent = true })

Map("n", "dp", "<plug>Dsurround")
Map("n", "yp", "<plug>Ysurround")
Map("n", "yP", "<plug>YSurround")
Map("n", "cp", "<plug>Csurround")
Map("n", "cP", "<plug>CSurround")
Map("x", "P", "<plug>Vsurround")

Map({ "n", "x", "o" }, "$w", "<plug>(WordMotion_w)")
Map({ "n", "x", "o" }, "$b", "<plug>(WordMotion_b)")
Map({ "n", "x", "o" }, "$e", "<plug>(WordMotion_e)")
Map({ "n", "x", "o" }, "$ge", "<plug>(WordMotion_ge)")

-- GOTO
Map({ "n", "x", "o" }, "gg", "gg")
Map({ "n", "x", "o" }, "gj", "G")
Map({ "n", "x", "o" }, "gk", "gg")
Map({ "n", "x", "o" }, "gh", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], { expr = true })
Map({ "n", "x", "o" }, "gl", [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], { expr = true })

Map({ "n", "x", "o" }, "gt", "H")
Map({ "n", "x", "o" }, "gm", "M")
Map({ "n", "x", "o" }, "gb", "L")

Map("n", "gf", "gf")
Map("n", "gF", ":edit <cfile><cr>")
Map("n", "gx", ":!xdg-open <cfile> &<cr><cr>")

Map("n", "gs", "<cmd>Telescope lsp_workspace_symbols theme=get_ivy<cr>")
Map("n", "gS", "<cmd>Telescope lsp_document_symbols theme=get_ivy<cr>")
Map("n", "ge", "<cmd>Telescope diagnostics theme=get_ivy<cr>")
Map("n", "gr", "<cmd>Telescope lsp_references theme=get_ivy<cr>")
Map("n", "gI", "<cmd>Telescope lsp_implementations theme=get_ivy<cr>")
Map("n", "gD", "<cmd>Telescope lsp_type_definitions theme=get_ivy<cr>")
Map("n", "go", vim.lsp.buf.outgoing_calls)
Map("n", "gi", vim.lsp.buf.incoming_calls)

-- VIEW
Map({ "n", "x" }, "vt", "zt")
Map({ "n", "x" }, "vv", "zz")
Map({ "n", "x" }, "vc", "zz")
Map({ "n", "x" }, "vb", "zb")

Map({ "n", "x" }, "vf", "zs")
Map({ "n", "x" }, "vm", "<cmd>set sidescrolloff=999<cr><cmd>set sidescrolloff=0<cr>")
Map({ "n", "x" }, "ve", "ze")

Map({ "n", "x" }, "vh", "zh")
Map({ "n", "x" }, "vl", "zl")
Map({ "n", "x" }, "vj", "<c-e>")
Map({ "n", "x" }, "vk", "<c-y>")

Map({ "n", "x" }, "vu", "<c-u>")
Map({ "n", "x" }, "vd", "<c-d>")

Map({ "n", "x" }, "vs", "<cmd>normal! HVL<cr>")

Map({ "n", "x" }, "m", "v")
Map({ "n", "x" }, "mk", "<m-v>{")
Map({ "n", "x" }, "mh", "<m-v>(")
Map({ "n", "x" }, "ml", "<m-v>)")
Map({ "n", "x" }, "mj", "<m-v>}")
Map({ "n", "x" }, "mi", "<m-v>i")
Map({ "n", "x" }, "ma", "<m-v>a")

Map("n", ",.", vim.lsp.buf.code_action)
Map("x", ",.", vim.lsp.buf.range_code_action)

Map("n", ",j", "m1J`1")
Map("n", ",k", "i<cr><esc>")
Map("x", ",j", "J")
Map("x", ",k", "c<cr><esc>")

Map({ "n", "x" }, "R", "<plug>(SubversiveSubstitute)")

Map("n", ",rr", vim.lsp.buf.rename)

Map("x", "<leader>re", function()
    require("refactoring").refactor("Extract Function")
end)
Map("x", "<leader>rf", function()
    require("refactoring").refactor("Extract Function to File")
end)
Map("x", "<leader>rv", function()
    require("refactoring").refactor("Extract Variable")
end)
Map("x", "<leader>ri", function()
    require("refactoring").refactor("Inline Variable")
end)
Map("n", "<leader>re", "zii<cmd>lua require('refactoring').refactor('Extract Function')<cr>")
Map("n", "<leader>rf", "zii<cmd>lua require('refactoring').refactor('Extract Function to File')<cr>")
Map("n", "<leader>rv", "zi,<cmd>lua require('refactoring').refactor('Extract Variable')<cr>")
Map("n", "<leader>ri", "zi,<cmd>lua require('refactoring').refactor('Inline Variable')<cr>")
Map("n", "<leader>rd", "<cmd>Neogen<cr>")

Map({ "n", "x" }, ",t", "<Plug>(EasyAlign)")

Map("n", ",ff", vim.lsp.buf.format)
Map("n", ",f<space>", [[<cmd>%s/\v[^^ ]\zs  / /g<cr>]])
Map("n", ",fw", "m1!ippar w100<cr>`1", { silent = true })
Map("n", ",fW", [["<cmd>%!par w" . &textwidth . "<cr>"]], { expr = true })
Map("x", ",ff", vim.lsp.buf.range_formatting)
Map("x", ",f<space>", ":%s/\v[^^ ]\zs  / /g<cr>")
Map("x", ",fw", [["!par w" . &textwidth . "<cr>"]], { expr = true })

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

Map("n", ",sp", "<Plug>CaserMixedCase", { remap = true })
Map("n", ",sc", "<Plug>CaserCamelCase", { remap = true })
Map("n", ",ss", "<Plug>CaserSnakeCase", { remap = true })
Map("n", ",su", "<Plug>CaserUpperCase", { remap = true })
Map("n", ",st", "<Plug>CaserTitleCase", { remap = true })
Map("n", ",sd", "<Plug>CaserSentenceCase", { remap = true })
Map("n", ",s<space>", "<Plug>CaserSpaceCase", { remap = true })
Map("n", ",sk", "<Plug>CaserKebabCase", { remap = true })
Map("n", ",sk", "<Plug>CaserTitleKebabCase", { remap = true })
Map("n", ",s.", "<Plug>CaserDotCase", { remap = true })
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

Map("n", "<leader><leader>", "<cmd>e #<cr>")
Map("n", "<leader>.", vim.lsp.buf.code_action)
Map("x", "<leader>.", vim.lsp.buf.range_code_action)

Map("n", "<leader>//", "<cmd>A<cr>")
Map("n", "<leader>/r", "<cmd>Ereadme<cr>")

Map("n", "<leader>f", "<cmd>call v:lua.project_files()<cr>")
Map("n", "<leader>F", "<cmd>Telescope resume<cr>")

nnoremap("<leader>gr", "<cmd>Gitsigns reset_hunk<CR>")
nnoremap("<leader>gs", "<cmd>Gitsigns stage_hunk<CR>")

nnoremap("<leader>gdh", "<cmd>let g:DiffviewLast='DiffviewFileHistory'<cr><cmd>DiffviewFileHistory<CR>")
Map("n", "<leader>gdH", [["<cmd>DiffviewFileHistory" . getcwd() . "<CR>"]], { expr = true })

-- mapxName.name("<leader>j", "Debugging")
-- nnoremap("<leader>J", "<cmd>tabedit %<cr><cmd>lua require'dapui'.open()<cr>", "Open Debug Panels")
-- nnoremap("<leader>jo", "<cmd>tabedit %<cr><cmd>lua require'dapui'.open()<cr>", "Open Debug Panels")
-- nnoremap("<leader>jn", function() dap.step_over() end, "Step to the Next Line")
-- nnoremap("<leader>jN", function() dap.continue() end, "Step to the Next Breakpoint")
-- nnoremap("<leader>ji", function() dap.step_into() end, "Step In")
-- nnoremap("<leader>jo", function() dap.step_out() end, "Step Out")
-- nnoremap("<leader>jr", function() dap.step_back() end, "Reverse")
-- nnoremap("<leader>jc", function() dap.continue() end, "Run to Cursor")
-- nnoremap("<leader>jl", function() dap.repl.toggle() end, "Toggle REPL")
-- nnoremap("<leader>jq", function() dap.close() end, "Quit")
-- nnoremap("<leader>jb", function() dap.toggle_breakpoint() end, "Set Breakpoint")
-- nnoremap("<leader>je", function() require'dapui'.eval() end, "Eval Exression")
-- mapxName.name("<leader>jp", "Print Debugging")
--     nnoremap("<leader>jd", function() require('refactoring').debug.printf({})end, "Printf")
--     xnoremap("<leader>jd", function() require('refactoring').debug.print_var({})end, "Print Var")
--     nnoremap("<leader>jq", function() require('refactoring').debug.cleanup({})end, "Cleanup")

-- Git Diff Bindings
if vim.api.nvim_win_get_option(0, "diff") then
    Map("n", "<leader>[", "<cmd>diffget LOCAL<cr>", "Take From Local Change")
    Map("n", "<leader>]", "<cmd>diffget REMOTE<cr>", "Take From Remote Change")
    Map("n", "<leader><leader>", "<cmd>diffget BASE<cr>", "Take From Base")
end

-- Insert Bindings

Map({ "i", "s", "c" }, "<c-a>", "<HOME>")
Map({ "i", "s", "c" }, "<c-e>", "<END>")

Map({ "i", "s" }, "<c-]>", "<plug>luasnip-next-choice")
Map({ "i", "s", "c" }, "<c-space>", "v:lua.cmp_toggle()", { expr = true })

noremap("<c-leftmouse>", "<cmd>Telescope lsp_definitions theme=get_ivy<cr>")
noremap("<c-rightmouse>", "gf")

Ls = require("luasnip")
Map("n", "<leader>,s", "<cmd>source ~/.config/nvim/after/plugin/luasnip.lua<cr>")
Map("n", "<leader>,S", "<cmd>vsplit ~/.config/nvim/after/plugin/luasnip.lua<cr>")

Map({ "i", "s" }, "<tab>", function()
    if Ls.expand_or_jumpable() then
        Ls.expand_or_jump()
    end
end, { silent = true })

Map({ "i", "s" }, "<s-tab>", function()
    if Ls.jumpable(-1) then
        Ls.jump(-1)
    end
end, { silent = true })

Map("i", "<c-n>", function()
    if Ls.choice_active() then
        Ls.change_choice(1)
    end
end)

-- Command Panel Bindings

GlobalCommands = {
    { name = "Lazygit", command = "silent !kitty @ launch --cwd=current --type=tab --tab-title 'LazyGit' lazygit" },

    { name = "Quickfix", command = "Telescope quickfix theme=get_ivy" },
    { name = "Todo list", command = "TodoTelescope theme=get_ivy" },
    { name = "Undo tree", command = "UndotreeToggle" },

    { name = "Grep", command = "Telescope live_grep theme=get_ivy theme=get_ivy" },
    { name = "Buffers", command = "Telescope buffers theme=get_ivy" },
    { name = "Old files finder", command = "Telescope oldfiles theme=get_ivy" },
    { name = "Diagnostics", command = "Telescope diagnostics bufnr=0 theme=get_ivy" },
    { name = "Workspace diagnostics", command = "Telescope diagnostics theme=get_ivy" },
    { name = "Symbols", command = "Telescope lsp_document_symbols theme=get_ivy" },
    { name = "Workspace symbols", command = "Telescope lsp_workspace_symbols theme=get_ivy" },
    { name = "Notifications", command = "Telescope notify theme=get_ivy" },
    { name = "Files", command = "Telescope git_file theme=get_ivy" },
    { name = "File browser", command = "Telescope file_browser respect_gitignore=fals theme=get_ivy" },
    {
        name = "File browser (relative)",
        func = function()
            require("telescope").extensions.file_browser.file_browser({
                cwd = require("telescope.utils").buffer_dir(),
                respect_gitignore = false,
            })
        end,
    },

    { name = "Reset File", command = "Gitsigns reset_buffer" },
    { name = "Stage File", command = "Gitsigns stage_buffer" },

    { name = "File history", command = "DiffviewFileHistory" },
    { name = "Diff of unstaged", command = "DiffviewOpen" },
    {
        name = "Diff of a commit",
        func = function()
            git_commits_onechange()
        end,
    },
    {
        name = "Diff against a commit",
        func = function()
            git_commits_againsthead()
        end,
    },
    {
        name = "Diff of a branch from current",
        func = function()
            git_branch_dif()
        end,
    },
    {
        name = "Diff of a branch from master",
        func = function()
            git_branch_mergebase()
        end,
    },

    { name = "Close tab", command = "tabclose" },
    { name = "Toggle text wraping", "set wrap!" },
    { name = "Clear search", command = "let @/=''" },
}

mapxName.name("<leader>l", "ProfiLe")
nnoremap("<leader>le", "<cmd>PerfPickEvent<cr>")
nnoremap("<leader>la", "<cmd>PerfAnnotate<cr>")
nnoremap("<leader>lf", "<cmd>PerfAnnotateFunction<cr>")
xnoremap("<leader>la", "<cmd>PerfAnnotateSelection<cr>")
nnoremap("<leader>lt", "<cmd>PerfToggleAnnotations<cr>")
nnoremap("<leader>lh", "<cmd>PerfHottestLines<cr>")
nnoremap("<leader>ls", "<cmd>PerfHottestSymbols<cr>")
nnoremap("<leader>lc", "<cmd>PerfHottestCallersFunction<cr>")
xnoremap("<leader>lc", "<cmd>PerfHottestCallersSelection<cr>")
mapxName.name("<leader>lo", "Open Profile Data")
nnoremap("<leader>lof", "<cmd>PerfLoadFlat<cr>")
nnoremap("<leader>log", "<cmd>PerfLoadCallGraph<cr>")
nnoremap("<leader>loo", "<cmd>PerfLoadFlameGraph<cr>")
nnoremap("<leader>loc", "<cmd>PerfCycleFormat<cr>")

Map("n", "<leader>p", function()
    CommandCentre()
end)
function CommandCentre(argCommands)
    local commands = {}
    if argCommands == nil then
        for _, v in pairs(GlobalCommands) do
            table.insert(commands, v)
        end
        if vim.b[0].localCommands ~= nil then
            for _, v in pairs(vim.b[0].localCommands) do
                table.insert(commands, v)
            end
        end
    else
        commands = argCommands
    end

    vim.ui.select(commands, {
        prompt = "Select Commands",
        format_item = function(item)
            return item.name
        end,
    }, function(choice)
        if not choice then
            return
        end

        if choice.func ~= nil then
            choice.func()
        elseif choice.command ~= nil then
            vim.cmd(choice.command)
        elseif choice.keymap ~= nil then
            vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(choice.keymap, true, true, true), "n", false)
        else
            return
        end
    end)
end
