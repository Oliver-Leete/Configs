require("which-key").register({
    ["<c-]>"] = { "<plug>luasnip-next-choice", "#todo", noremap = false },
    -- ["<cr>"] = { "v:lua.enter_complete()", "#todo", expr = true, noremap = false },
    ["<c-space>"] = { "v:lua.cmp_toggle()", "#todo", expr = true, noremap = false },
    ["<S-Tab>"] = { "v:lua.s_tab_complete()", "#todo", expr = true, noremap = false },
    ["<Tab>"] = { "v:lua.tab_complete()", "#todo", expr = true, noremap = false },
    ["<c-a>"] = { "<C-O>^", "Home" },
    ["<c-e>"] = { "<End>", "End" },
    -- ["<esc>"] = { "v:lua.cmp_esc()", "Escape or Close Compleation", expr=true, noremap = false},
}, {
    mode = "i",
})

require("which-key").register({
    ["<c-]>"] = { "<plug>luasnip-next-choice", "#todo", noremap = false },
    -- ["<cr>"] = { "v:lua.enter_complete()", "#todo", expr = true, noremap = false },
    ["<c-space>"] = { "v:lua.cmp_toggle()", "#todo", expr = true, noremap = false },
    ["<S-Tab>"] = { "v:lua.s_tab_complete()", "#todo", expr = true, noremap = false },
    ["<Tab>"] = { "v:lua.tab_complete()", "#todo", expr = true, noremap = false },
    -- ["<esc>"] = { "v:lua.cmp_esc()", "Escape or Close Compleation", expr=true, noremap = false},
}, {
    mode = "s",
})

require("which-key").register({
    ["<c-a>"] = { "<Home>", "Home" },
    ["<c-e>"] = { "<End>", "End" },
}, {
    mode = "c",
})
