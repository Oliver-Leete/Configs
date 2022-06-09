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
Map("x", "<m-o>", "<esc>`>o<esc>gv")
Map("x", "<m-O>", "<esc>`<O<esc>gv")

Map("x", "I", "I")
Map("x", "A", "A")

Map("n", "<c-/>", ",cc", { remap = true })
Map("x", "<c-/>", ",c", { remap = true })

Map("n", "£", [[:exe "let @/='" . expand("<cWORD>") . "' "<cr>]], { silent = true })

-- Panel Specific Mappings
local panelMappings = vim.api.nvim_create_augroup("panelMappings", { clear = true })
vim.api.nvim_create_autocmd("filetype", {
    pattern = { "qf", "help", "vim-plug", "juliadoc", "lspinfo", "tsplayground", "harpoon-menu" },
    callback = function() Map("n", "<esc>", "<cmd>q<cr>", { buffer = 0 }) end,
    group = panelMappings,
})
vim.api.nvim_create_autocmd("BufEnter", {
    callback = function()
        if vim.bo.buftype == "nofile" and vim.bo.filetype == "" then
            Map("n", "<esc>", "<cmd>q<cr>", { buffer = 0 })
        end
    end,
    group = panelMappings,
})
vim.api.nvim_create_autocmd("filetype", {
    pattern = "undotree",
    callback = function() Map("n", "<esc>", "<cmd>UndotreeHide<cr>", { buffer = 0 }) end,
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

Map({ "n", "x", "o" }, "j", [[v:count?(v:count>5?"m'".v:count:'').'j':'gj']], { expr = true })
Map({ "n", "x", "o" }, "k", [[v:count?(v:count>5?"m'".v:count:'').'k':'gk']], { expr = true })

Map({ "n", "x", "o" }, "H", [[getline('.')[0:col('.')-2]=~#'^\s\+$'?'0':'^']], { expr = true })
Map({ "n", "x" }, "L", [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], { expr = true })
Map("o", "L", "$")

Map("n", "J", "gi")
Map({ "n", "x" }, "U", "<c-r>")

Map("n", "K", "<nop>")
Map("n", "KK", "K")
Map("n", "KG", require("gitsigns").preview_hunk)
Map("n", "KA", function() require("gitsigns").blame_line({ full = true }) end)
Map("n", "KE", function() vim.diagnostic.open_float(0, { border = "single", scope = "line", source = "always" }) end)

Map("x", "J", ":move '>+1<cr>gv=gv")
Map("x", "K", ":move '<-2<cr>gv=gv")

Map("n", "Q", "@q")
Map("x", "Q", ":norm! @q<cr>")

Map("n", "s", require("hop").hint_char1)
Map("n", "S", "<cmd>ISwapWith<cr>")
Map({ "x", "o" }, "s", function() require("hop").hint_char1({ inclusive_jump = true }) end)

Map({ "n", "x", "o" }, "'", "`")
Map({ "n", "x", "o" }, "`", "'")

Map("x", "<", "<gv")
Map("x", ">", ">gv")

Map("n", "<c-v>", "<cmd>vsplit %<cr>")
Map("n", "<c-x>", "<cmd>split %<cr>")
Map("n", "<c-p>", "<cmd>pedit %<cr>")

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
Map({ "n", "x" }, "gl", [[getline('.')[col('.'):-1]=~#'^\s\+$'?'$':'g_']], { expr = true })
Map("o", "gl", "$")

Map({ "n", "x", "o" }, "gt", "H")
Map({ "n", "x", "o" }, "gm", "M")
Map({ "n", "x", "o" }, "gb", "L")

Map("n", "gf", "gf")
Map("n", "gF", ":edit <cfile><cr>")
Map("n", "gx", ":!xdg-open <cfile> &<cr><cr>")

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

Map("n", ",j", "m1J`1")
Map("n", ",k", require("trevj").format_at_cursor)
Map("x", ",j", "J")

Map({ "n", "x" }, "R", "<plug>(SubversiveSubstitute)")

Map("n", ",rr", vim.lsp.buf.rename)

Map("n", ",rf", function() require("refactoring").refactor("Extract Block") end)
Map("x", ",rf", function() require("refactoring").refactor("Extract Function") end)
Map("n", ",rF", function() require("refactoring").refactor("Extract Block to File") end)
Map("x", ",rF", function() require("refactoring").refactor("Extract Function to File") end)
Map("n", ",re", "mi,:lua require('refactoring').refactor('Extract Variable')<cr>", { remap = true })
Map("x", ",re", function() require("refactoring").refactor("Extract Variable") end)
Map({ "n", "x" }, ",ri", function() require("refactoring").refactor("Inline Variable") end)

Map("n", ",rd", "<cmd>Neogen<cr>")
Map("n", ",ra", ",cA", { remap = true })

Map("n", ",dd", function() require("refactoring").debug.printf({}) end)
Map({ "n", "x" }, ",dv", function() require("refactoring").debug.print_var({}) end, { remap = false })
Map("n", ",dq", function() require("refactoring").debug.cleanup({}) end)

Map("n", ",s", "<Plug>SortMotion", { remap = true })
Map("n", ",ss", "<Plug>SortLines", { remap = true })
Map("x", ",s", "<Plug>SortMotionVisual", { remap = true })

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

Map("n", "<leader>n", require("harpoon.mark").add_file)
Map("n", "<leader>e", require("harpoon.ui").toggle_quick_menu)
local harpoon_keys = { "a", "r", "s", "t" }
for i, key in pairs(harpoon_keys) do
    Map("n", "<leader>" .. key, function() require("harpoon.ui").nav_file(i) end)
end

Map("n", "<leader>//", "<cmd>A<cr>")
Map("n", "<leader>/r", "<cmd>Ereadme<cr>")

Map("n", "<leader>f", "<cmd>call v:lua.project_files()<cr>")
Map("n", "<leader>F", "<cmd>Telescope resume<cr>")

Map("n", ",gr", "<cmd>Gitsigns reset_hunk<CR>")
Map("n", ",gs", "<cmd>Gitsigns stage_hunk<CR>")

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

-- Git Diff Bindings
if vim.api.nvim_win_get_option(0, "diff") then
    Map("n", "<leader>[", "<cmd>diffget LOCAL<cr>", "Take From Local Change")
    Map("n", "<leader>]", "<cmd>diffget REMOTE<cr>", "Take From Remote Change")
    Map("n", "<leader><leader>", "<cmd>diffget BASE<cr>", "Take From Base")
end

-- Insert Bindings

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

Map({ "i", "s", "c" }, "<c-a>", "<HOME>")
Map({ "i", "s", "c" }, "<c-e>", "<END>")

Map({ "i", "s" }, "<c-]>", "<plug>luasnip-next-choice")
Map({ "i", "s", "c" }, "<c-space>", "v:lua.cmp_toggle()", { expr = true })

Map("n", "<c-leftmouse>", "<cmd>Telescope lsp_definitions theme=get_ivy<cr>")
Map("n", "<c-rightmouse>", "gf")

Ls = require("luasnip")
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
    { source = "coverage", name = "Coverage summary", command = "CoverageSummary" },
    { source = "coverage", name = "Load coverage", command = "Coverage" },
    { source = "coverage", name = "Toggle coverage", command = "CoverageToggle" },

    { source = "default", name = "Close buffer", command = "bdelete!" },
    { source = "default", name = "Clear search", command = "let @/=''" },
    { source = "default", name = "Close tab", command = "tabclose" },
    { source = "default", name = "Toggle text wraping", "set wrap!" },
    { source = "default", name = "File tree", command = "NvimTreeToggle" },
    { source = "default", name = "Undo tree", command = "UndotreeToggle" },
    { source = "default", name = "Reload Snippets", command = "source ~/.config/nvim/after/plugin/luasnip.lua" },
    { source = "default", name = "Reload Snippets", command = "vsplit ~/.config/nvim/after/plugin/luasnip.lua" },

    { source = "finders", name = "Buffers", command = "Telescope buffers theme=get_ivy" },
    { source = "finders", name = "Diagnostics", command = "Telescope diagnostics bufnr=0 theme=get_ivy" },
    { source = "finders", name = "File browser (relative)", command = "Telescope file_browser respect_gitignore=false theme=get_ivy cwd=%:p:h" },
    { source = "finders", name = "Files", command = "Telescope git_files theme=get_ivy" },
    { source = "finders", name = "File browser", command = "Telescope file_browser respect_gitignore=false theme=get_ivy" },
    { source = "finders", name = "Grep", command = "Telescope live_grep theme=get_ivy theme=get_ivy" },
    { source = "finders", name = "Notifications", func = function() require("telescope").extensions.notify.notify(require("telescope.themes").get_ivy()) end },
    { source = "finders", name = "Old files finder", command = "Telescope oldfiles theme=get_ivy" },
    { source = "finders", name = "Quickfix", command = "Telescope quickfix theme=get_ivy" },
    { source = "finders", name = "Symbols", command = "Telescope lsp_document_symbols theme=get_ivy" },
    { source = "finders", name = "Todo list", command = "TodoTelescope theme=get_ivy" },
    { source = "finders", name = "Workspace diagnostics", command = "Telescope diagnostics theme=get_ivy" },
    { source = "finders", name = "Workspace symbols", command = "Telescope lsp_workspace_symbols theme=get_ivy" },

    { source = "git", name = "Diff against a commit", func = function() git_commits_againsthead() end, },
    { source = "git", name = "Diff of a branch from current", func = function() git_branch_dif() end, },
    { source = "git", name = "Diff of a branch from master", func = function() git_branch_mergebase() end, },
    { source = "git", name = "Diff of a commit", func = function() git_commits_onechange() end, },
    { source = "git", name = "Diff of unstaged", command = "DiffviewOpen" },
    { source = "git", name = "File history", command = "DiffviewFileHistory" },
    {
        source = "git",
        name = "Lazygit",
        command = "silent !kitty @ launch --cwd=current --type=tab --tab-title 'LazyGit' lazygit",
    },
    { source = "git", name = "Reset File", command = "Gitsigns reset_buffer" },
    { source = "git", name = "Stage File", command = "Gitsigns stage_buffer" },

    { source = "profiling", name = "Profile Annotate Function", command = "PerfAnnotateFunction" },
    { source = "profiling", name = "Profile Cycle Format", command = "PerfCycleFormat" },
    { source = "profiling", name = "Profile Hottest Callers Function", command = "PerfHottestCallersFunction" },
    { source = "profiling", name = "Profile Hottest Callers Selection", command = "PerfHottestCallersSelection" },
    { source = "profiling", name = "Profile Hottest Lines", command = "PerfHottestLines" },
    { source = "profiling", name = "Profile Hottest Symbols", command = "PerfHottestSymbols" },
    { source = "profiling", name = "Profile Load Call Graph", command = "PerfLoadCallGraph" },
    { source = "profiling", name = "Profile Load Flame Graph", command = "PerfLoadFlameGraph" },
    { source = "profiling", name = "Profile Load Flat", command = "PerfLoadFlat" },
    { source = "profiling", name = "Profile Pick Event", command = "PerfPickEvent" },
    { source = "profiling", name = "Profile Toggle Annotations", command = "PerfToggleAnnotations" },

    { source = "tasks", name = "Tasks", func = Select_runnables },
}

Map("n", "<leader>p", function() CommandCentre() end)

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

    table.sort(commands, function(a, b)
        return a.name < b.name
    end)

    table.sort(commands, function(a, b)
        return a.source < b.source
    end)

    vim.ui.select(commands, {
        prompt = "Command Centre",
        format_item = function(item)
            return "[" .. item.source:sub(1, 3) .. "] " .. item.name
        end,
        telescope = require("telescope.themes").get_ivy(),
    }, function(choice)
        if not choice then
            pcall(vim.notify("No command entered", "warn", { title = "Command Centre" }))
            return
        end

        if choice.func ~= nil then
            choice.func()
        elseif choice.command ~= nil then
            vim.cmd(choice.command)
        elseif choice.keymap ~= nil then
            vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(choice.keymap, true, true, true), "n", false)
        else
            pcall(vim.notify("Command does not have an action", "warn", { title = "Command Centre" }))
            return
        end
    end)
end

Global_Runnables = function()
    return {}
end

function Select_runnables()
    local runnables = {}

    if Global_Runnables then
        for _, v in pairs(Global_Runnables()) do
            table.insert(runnables, v)
        end
    end

    if vim.b[0].runnables then
        for _, v in pairs(vim.b[0].runnables()) do
            table.insert(runnables, v)
        end
    end

    local handle1 = io.popen([[fd -I tasks.lua]])
    local task_files
    if handle1 then
        task_files = handle1:read("*a")
        handle1:close()
    end

    if task_files then
        local project_tasks
        for name in task_files:gmatch("([^\r\n]+)") do
            name = name:gsub("%./", ""):gsub("%.lua", "")
            project_tasks = require(name)
            if type(project_tasks) == "table" then
                for _, v in pairs(project_tasks) do
                    table.insert(runnables, v)
                end
            end
        end
    end

    if #runnables ~= 0 then
        CommandCentre(runnables)
    else
        pcall(vim.notify("Nothing to Run", "warn", { title = "Command Centre" }))
    end
end

Map("n", "<leader>d", Select_runnables)
