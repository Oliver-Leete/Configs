local s = Ls.snippet
local t = Ls.text_node
local i = Ls.insert_node

return {
}, {
    s({ trig = "-d", name = "doc string" }, {
        t({ '"""' }), i(0), t({ '"""' })
    }),
}
