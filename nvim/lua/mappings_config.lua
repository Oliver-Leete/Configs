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

-- NOTE: _, =, |, ;, ^, BS are free to map

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

-- UnMap Plugins
vim.g.kitty_navigator_no_mappings = true
vim.g.julia_blocks = false

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
Map("n", "KT", function() require("neotest").output.open() end)

Map("x", "J", ":move '>+1<cr>gv=gv")
Map("x", "K", ":move '<-2<cr>gv=gv")

Map("n", "Q", "@q")
Map("x", "Q", ":norm! @q<cr>")

Map("n", "S", "<cmd>ISwapWith<cr>")

Map({ "n", "x", "o" }, "s", require("hop").hint_char1)

Map({ "n", "x", "o" }, "'", "`")
Map({ "n", "x", "o" }, "`", "'")

Map("x", "<", "<gv")
Map("x", ">", ">gv")

Map("n", "<c-v>", "<cmd>silent vsplit %<cr>")
Map("n", "<c-x>", "<cmd>silent split %<cr>")

local get_input = function(prompt)
    local ok, result = pcall(vim.fn.input, { prompt = prompt })
    if not ok then
        return nil
    end
    return result
end

-- require("nvim-surround").setup({
--     keymaps = {
--         normal = "yp",
--         delete = "dp",
--         change = "cp",
--         visual = "P",
--     },
--     delimiters = {
--         pairs = {
--             ["("] = { "(", ")" },
--             [")"] = { "( ", " )" },
--             ["{"] = { "{", "}" },
--             ["}"] = { "{ ", " }" },
--             ["["] = { "[", "]" },
--             ["]"] = { "[ ", " ]" },
--             ["<"] = { "<", ">" },
--             [">"] = { "< ", " >" },
--             ["i"] = function()
--                 return {
--                     get_input( "Enter the left delimiter: "),
--                     get_input( "Enter the right delimiter: ")
--                 }
--             end,
--             ["f"] = function()
--                 return {
--                     get_input( "Enter the function name: ") .. "(", ")"
--                 }
--             end,
--         },
--         aliases = {
--             ["a"] = false,
--             ["b"] = { '[', "{", "(" },
--             ["q"] = { '"', "'", "`" },
--         },
--     }
-- })
--
vim.g.wordmotion_nomap = true
Map({ "n", "x", "o" }, "<m-w>", "<plug>(WordMotion_w)", { remap = true })
Map({ "n", "x", "o" }, "<m-b>", "<plug>(WordMotion_b)", { remap = true })
Map({ "n", "x", "o" }, "<m-e>", "<plug>(WordMotion_e)", { remap = true })
Map({ "n", "x", "o" }, "<m-g>e", "<plug>(WordMotion_ge)", { remap = true })
Map({ "n", "x", "o" }, "<m-g><m-e>", "<plug>(WordMotion_ge)", { remap = true })
Map({ "x", "o" }, "i<m-w>", "<plug>(WordMotion_iw)", { remap = true })
Map({ "x", "o" }, "a<m-w>", "<plug>(WordMotion_aw)", { remap = true })

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
Hydra({
    name = "View",
    mode = { "n", "x" },
    body = "v",
    config = {
        hint = {
            position = "middle-right",
            border = "single"
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
        { "m", "<cm>set sidescrolloff=999<cr><cmd>set sidescrolloff=0<cr>", { exit = true } },
        { "e", "ze", { exit = true } },
    }
})
Map({ "n", "x" }, "vt", "zt")
Map({ "n", "x" }, "vv", "zz")
Map({ "n", "x" }, "vb", "zb")
Map({ "n", "x" }, "vs", "zs")
Map({ "n", "x" }, "vm", "<cm>set sidescrolloff=999<cr><cmd>set sidescrolloff=0<cr>")
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
Map("n", ",re", "mi,:lua require('refactoring').refactor('Extract Variable')<cr>", { remap = true })
Map("x", ",re", function() require("refactoring").refactor("Extract Variable") end)
Map({ "n", "x" }, ",ri", function() require("refactoring").refactor("Inline Variable") end)

Map("n", ",rd", "<cmd>Neogen<cr>")
-- Map("n", ",ra", ",cA", { remap = true })

Map("n", ",dd", function() require("refactoring").debug.printf({}) end)
Map({ "n", "x" }, ",dv", function() require("refactoring").debug.print_var({}) end, { remap = false })
Map("n", ",dq", function() require("refactoring").debug.cleanup({}) end)

Map({ "n", "x" }, ",s", "<Plug>Opsort", { remap = true })
Map("n", ",ss", "<Plug>OpsortLines", { remap = true })
-- Map("x", ",s", "<Plug>Opsort", { remap = true })

Map({ "n", "x" }, ",t", "<Plug>(EasyAlign)")

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

Map("n", ",ff", vim.lsp.buf.format)
Map("n", ",f<space>", [[<cmd>%s/\v[^^ ]\zs  / /g<cr>]])
Map("n", ",fw", "m1!ippar w100<cr>`1", { silent = true })
Map("n", ",fW", [["<cmd>%!par w" . &textwidth . "<cr>"]], { expr = true })
Map("x", ",ff", vim.lsp.buf.range_formatting)
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

Map("n", "<leader>f", "<cmd>call v:lua.project_files()<cr>")
Map("n", "<leader>F", "<cmd>Telescope resume<cr>")

Map("n", ",gr", "<cmd>Gitsigns reset_hunk<CR>")
Map("n", ",gs", "<cmd>Gitsigns stage_hunk<CR>")

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

Map("i", ",", ",<c-g>u")
Map("i", ".", ".<c-g>u")
Map("i", "!", "!<c-g>u")
Map("i", "?", "?<c-g>u")

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

-- Terminal mappings

local term_keys = { "n", "e", "i", "o" }
for i, key in pairs(term_keys) do
    Map("n", "<cr>" .. key, function() _G.sendLines(vim.v.count, i) end)
    Map("x", "<cr>" .. key, ":<c-u>call v:lua.sendRegion(visualmode(), " .. i .. ")<cr>", { remap = true })
end

Map("n", "<leader>n", function() Harp_Term_1:toggle() end)
Map("n", "<leader>e", function() Harp_Term_2:toggle() end)
Map("n", "<leader>i", function() Harp_Term_3:toggle() end)
Map("n", "<leader>o", function() Harp_Term_4:toggle() end)

Map("n", "<leader>:", function() CommandCentre(Background_Term_List) end)

Map("t", "<c-]>", "<c-\\><c-n>")

-- Command Panel Bindings

GlobalCommands = {
    { source = "coverage", name = "Coverage summary", command = "CoverageSummary" },
    { source = "coverage", name = "Load coverage", command = "Coverage" },
    { source = "coverage", name = "Toggle coverage", command = "CoverageToggle" },

    { source = "default", name = "Close buffer", func = function() _G.delete_buffer() end },
    { source = "default", name = "Clear search", command = "let @/=''" },
    { source = "default", name = "Close tab", command = "tabclose" },
    { source = "default", name = "Toggle text wraping", "set wrap!" },
    { source = "default", name = "File tree", command = "NvimTreeToggle" },
    { source = "default", name = "Undo tree", command = "UndotreeToggle" },
    { source = "default", name = "Reload snippets", command = "source ~/.config/nvim/after/plugin/luasnip.lua" },
    { source = "default", name = "Source init", command = "source /home/oleete/.config/nvim/init.lua" },

    { source = "log", name = "Neovim Log", func = function() NvimLogTerm:open_add(4) end },
    { source = "log", name = "LSP Log", func = function() LspLogTerm:open_add(4) end },
    { source = "log", name = "X Session Log", func = function() XLogTerm:open_add(4) end },

    { source = "finders", name = "Buffers", command = "Telescope buffers theme=get_ivy" },
    { source = "finders", name = "Diagnostics", command = "Telescope diagnostics bufnr=0 theme=get_ivy" },
    { source = "finders", name = "File browser (relative)",
        command = "Telescope file_browser respect_gitignore=false theme=get_ivy cwd=%:p:h" },
    { source = "finders", name = "Files", command = "Telescope git_files theme=get_ivy" },
    { source = "finders", name = "File browser", command = "Telescope file_browser respect_gitignore=false theme=get_ivy" },
    { source = "finders", name = "Grep", command = "Telescope live_grep theme=get_ivy theme=get_ivy" },
    { source = "finders", name = "Notifications",
        func = function() require("telescope").extensions.notify.notify(require("telescope.themes").get_ivy()) end },
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
    { source = "git", name = "File diff history", command = "DiffviewFileHistory %" },
    { source = "git", name = "Folder diff history", command = "DiffviewFileHistory" },
    { source = "git", name = "Lazygit", func = function() LZGTerm:toggle() end, },
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

Map("n", "<leader>p", function() CommandCentre({}, true) end)


local function append_command(runnables, to_add)
    local function always_extend(dst, src)
        if not vim.tbl_islist(src) then
            src = vim.tbl_values(src)
        end
        vim.list_extend(dst, src)
    end

    if to_add then
        if type(to_add) == "table" then
            always_extend(runnables, to_add)
        elseif type(to_add) == "function" then
            always_extend(runnables, to_add())
        end
    end
end

function CommandCentre(argCommands, extend)
    if not extend then extend = false end
    if not argCommands then argCommands = {} end

    local commands = {}
    local command_sources = { argCommands }

    if extend then
        local default_sources = { GlobalCommands, vim.b[0].localCommands }
        for _, source in pairs(default_sources) do
            table.insert(command_sources, source)
        end
    end

    for _, source in pairs(command_sources) do
        append_command(commands, source)
    end

    table.sort(commands, function(a, b) return a.name < b.name end)
    table.sort(commands, function(a, b) return a.source < b.source end)

    vim.ui.select(commands, {
        prompt = "Command Centre",
        format_item = function(item)
            return "[" .. item.source:sub(1, 3) .. "] " .. item.name
        end,
        telescope = require("telescope.themes").get_ivy(),
    }, function(choice)
        if not choice then
            vim.notify("No command entered", "warn", { title = "Command Centre" })
            return
        end

        if choice.func ~= nil then
            choice.func()
        elseif choice.command ~= nil then
            vim.cmd(choice.command)
        elseif choice.keymap ~= nil then
            vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(choice.keymap, true, true, true), "n", false)
        else
            vim.notify("Command does not have an action", "warn", { title = "Command Centre" })
            return
        end
    end)
end

Global_Runnables = {}


function Select_runnables()
    local runnables = {}

    -- Config sources
    local runnable_sources = { Global_Runnables, vim.g.runnables, vim.b[0].runnables }

    -- runnables from tasks.lua files in directory
    local handle1 = io.popen([[fd -I tasks.lua]])
    local task_files
    if handle1 then
        task_files = handle1:read("*a")
        handle1:close()
    end

    if task_files then
        for name in task_files:gmatch("([^\r\n]+)") do
            name = name:gsub("%./", ""):gsub("%.lua", "")
            table.insert(runnable_sources, require(name))
        end
    end

    for _, source in pairs(runnable_sources) do
        append_command(runnables, source)
    end

    if #runnables ~= 0 then
        CommandCentre(runnables)
    else
        vim.notify("Nothing to Run", "warn", { title = "Command Centre" })
    end
end

Map("n", "<leader>d", Select_runnables)
