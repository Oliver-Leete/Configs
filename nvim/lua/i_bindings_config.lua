require("which-key").register({
    ["<c-]>"] = { "<plug>luasnip-next-choice", "#todo", noremap = false },
    ["<CR>"] = {
        "<cmd>lua require'cmp'.confirm({ behavior = require'cmp'.ConfirmBehavior.Insert, select = false })<cr>",
        "#todo",
    },
    ["<c-space>"] = { "v:lua.cmp_toggle()", "#todo", expr = true, noremap = false },
    ["<S-Tab>"] = { "v:lua.s_tab_complete()", "#todo", expr = true, noremap = false },
    ["<Tab>"] = { "v:lua.tab_complete()", "#todo", expr = true, noremap = false },
    ["<c-a>"] = { "<C-O>^", "Home" },
    ["<c-e>"] = { "<End>", "End" },
    ["<Down>"] = { "<cmd>require'cmp'.select_next_item({ behavior = require'cmp'.SelectBehavior.Select })<cr>", "#todo" },
    ["<Up>"] = { "<cmd>require'cmp'.select_prev_item({ behavior = require'cmp'.SelectBehavior.Select }<cr>", "#todo" },
    -- ["<esc>"] = { "v:lua.cmp_esc()", "Escape or Close Compleation", expr=true, noremap = false},
}, {
    mode = "i",
})

require("which-key").register({
    ["<c-]>"] = { "<plug>luasnip-next-choice", "#todo", noremap = false },
    ["<CR>"] = {
        "<cmd>lua require'cmp'.confirm({ behavior = require'cmp'.ConfirmBehavior.Insert, select = false })<cr>",
        "#todo",
    },
    ["<c-space>"] = { "v:lua.cmp_toggle()", "#todo", expr = true, noremap = false },
    ["<S-Tab>"] = { "v:lua.s_tab_complete()", "#todo", expr = true, noremap = false },
    ["<Tab>"] = { "v:lua.tab_complete()", "#todo", expr = true, noremap = false },
    ["<Down>"] = { require("cmp").select_next_item({ behavior = require("cmp").SelectBehavior.Select }) },
    ["<Up>"] = { require("cmp").select_prev_item({ behavior = require("cmp").SelectBehavior.Select }) },
    -- ["<esc>"] = { "v:lua.cmp_esc()", "Escape or Close Compleation", expr=true, noremap = false},
}, {
    mode = "s",
})

require("which-key").register({
    ["<CR>"] = {
        "<cmd>lua require'cmp'.confirm({ behavior = require'cmp'.ConfirmBehavior.Insert, select = true })<cr><cr>",
        "#todo",
    },
    ["<c-a>"] = { "<Home>", "Home" },
    ["<c-e>"] = { "<End>", "End" },
}, {
    mode = "c",
})
