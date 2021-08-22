require("which-key").register({
    ["<c-]>"] = {"<plug>luasnip-next-choice", "#todo", noremap=false},
    ["<cr>"] = {"v:lua.enter_complete()", "#todo",  expr = true ,noremap=false},
    ["<c-space>"] = {"v:lua.compe_toggle()", "#todo", expr = true,noremap=false},
    ["<S-Tab>"] = {"v:lua.s_tab_complete()", "#todo",  expr = true ,noremap=false},
    ["<Tab>"] = {"v:lua.tab_complete()", "#todo",  expr = true ,noremap=false},
},{
    mode = "i"
})

require("which-key").register({
    ["<c-]>"] = {"<plug>luasnip-next-choice", "#todo", noremap=false},
    ["<cr>"] = {"v:lua.enter_complete()", "#todo",  expr = true ,noremap=false},
    ["<c-space>"] = {"v:lua.compe_toggle()", "#todo", expr = true,noremap=false},
    ["<S-Tab>"] = {"v:lua.s_tab_complete()", "#todo",  expr = true ,noremap=false},
    ["<Tab>"] = {"v:lua.tab_complete()", "#todo",  expr = true ,noremap=false},
},{
    mode = "s"
})
