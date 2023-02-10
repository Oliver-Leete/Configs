local s = Ls.snippet
-- local sn = Ls.snippet_node
-- local isn = Ls.indent_snippet_node
local t = Ls.text_node
local i = Ls.insert_node
local f = Ls.function_node
-- local c = Ls.choice_node
-- local d = Ls.dynamic_node
-- local r = Ls.restore_node
-- local events = require("luasnip.util.events")
-- local ai = require("luasnip.nodes.absolute_indexer")
-- local lse = require("luasnip.extras")
-- local fmt = require("luasnip.extras.fmt").fmt
-- local rep = lse.rep
-- local m = lse.match

-- local lse = require("luasnip.extras")
-- local l = lse.lambda
-- local r = lse.rep
-- local p = lse.partial
-- local n = lse.nonempty
-- local dl = lse.dynamic_lambda

return {},
    {
        s({ trig = "([^%s]*)@ref", name = "julia docstring reference", dscr = "Link to docstrings", regTrig = true, snippetType = "autosnippet" }, {
            t({ "[`" }), f(function(_, par) return par.snippet.captures[1] end, {}), t({ "`](@ref" }), i(1), t({ ")" }), i(0),
        }),
    }
