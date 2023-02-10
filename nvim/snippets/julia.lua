local s = Ls.snippet
local sn = Ls.snippet_node
-- local isn = Ls.indent_snippet_node
local t = Ls.text_node
local i = Ls.insert_node
local f = Ls.function_node
local c = Ls.choice_node
local d = Ls.dynamic_node
-- local r = Ls.restore_node
-- local events = require("luasnip.util.events")
-- local ai = require("luasnip.nodes.absolute_indexer")
local lse = require("luasnip.extras")
local fmt = require("luasnip.extras.fmt").fmt
local rep = lse.rep
-- local m = lse.match

-- local lse = require("luasnip.extras")
-- local l = lse.lambda
-- local r = lse.rep
-- local p = lse.partial
-- local n = lse.nonempty
-- local dl = lse.dynamic_lambda

local rec_elseif
rec_elseif = function()
    return sn(nil, c(1, { t({ "end" }),
            sn(nil, {
                t({ "else", "\t" }),
                i(1),
                t({ "", "end" })
            }),
            sn(nil, {
                t("elseif "), i(1, "condition"),
                t({ "", "\t" }), i(2), t({ "", "" }),
                d(3, rec_elseif, {})
            }), }))
end

local rec_exam
rec_exam = function()
    return sn(nil, c(1, { t(""),
            fmt([[
            juila>{}
            {}{}
            ]],
                {
                    i(2, "Command"),
                    i(3, "Return Value"), d(4, rec_exam, {})
                })
        })
        )
end

return {
        s({ trig = "docs", name = "documentation" }, {
            t({ '"""', "\t" }),
            i(0),
            t({ "", '"""' })
        }),
        s({ trig = "abst", name = "abstract" }, {
            t("abstract type "), i(0), t(" end")
        }),
        s({ trig = "bare", name = "baremodule" }, {
            t("baremodule "), i(1),
            t({ "", "\t" }), i(0),
            t({ "", "end" })
        }),
        s({ trig = "beg", name = "begin" }, {
            t({ "begin", "\t" }),
            i(0),
            t({ "", "end" })
        }),
        s({ trig = "do", name = "do" }, {
            t("do "), i(1),
            t({ "", "\t" }), i(0),
            t({ "", "end" })
        }),
        s({ trig = "for", name = "for" }, {
            t("for "), i(1), t(" in "), i(2),
            t({ "", "\t" }), i(3),
            t({ "", "end" }), i(0)
        }),
        s({ trig = "fun", name = "function", dscr = "function deffinition with ever expaing documentation" },
            fmt([[
"""
    {}({})

{}{}{}
"""
function {}({})
    {}
end{}
]], {
                rep(1), rep(2),
                i(3, "a short description"), d(4, function(args)
                local nodes = { t({ "", "", "# Arguments" }), i(1) }
                local args = vim.split(args[1][1], ",", false)
                for index, arg in ipairs(args) do
                    local arg = string.gsub(arg, "%s+", "")
                    table.insert(nodes, t({ "", "- `" .. arg .. "` : " }))
                    table.insert(nodes, i(index + 1))
                end
                return sn(nil, c(1, { t(""), sn(nil, nodes) }))
            end, { 2 }),
                c(5, {
                    t(""),
                    fmt([[


# Examples
```jldoctest
julia> {}
{}{}
```
]],
                        {
                            i(1, "Command"),
                            i(2, "Return Value"), d(3, rec_exam, {})
                        })
                }),
                i(1), i(2, "arguments"),
                i(6),
                i(0),
            })),
        s({ trig = "exam", name = "documentation example" }, {
            t({ "", "# Examples", "", "```jldoctest", "julia> " }),
            i(1, "command"), t({ "", "" }),
            -- i(2, "return"), t({"",""}), d(3, rec_exam, {}),
            t({ "", "", "```" })
        }),
        -- s({trig="args", name="documentation arguments"}, {
        --     t({"", "# Arguments", ""}),
        --     t("- `"), i(1, "args"), t("` :"), i(2, "discription"), t({"",""}), d(3, rec_args, {}),
        --     t({"", ""})
        -- }),
        s({ trig = "if", name = "if" }, {
            t("if "), i(1, "conditon"),
            t({ "", "\t" }), i(2),
            t({ "", "" }),
            c(3, {
                t("end"),
                sn(nil, {
                    t({ "else", "\t" }),
                    i(1),
                    t({ "", "end" })
                }),
                sn(nil, {
                    t("elseif "), i(1, "condition"),
                    t({ "", "\t" }), i(2), t({ "", "" }),
                    d(3, rec_elseif, {})
                }),
            }),
        }),
        s({ trig = "let", name = "let" }, {
            t("let "), i(1),
            t({ "", "\t" }), i(0),
            t({ "", "end" })
        }),
        s({ trig = "mac", name = "macro" }, {
            t("macro "), i(1), t("("), i(2),
            t({ ")", "\t" }), i(0),
            t({ "", "end" })
        }),
        s({ trig = "mod", name = "module" }, {
            t("module "), i(1),
            t({ "", "\t" }), i(0),
            t({ "", "end" })
        }),
        s({ trig = "mut", name = "mutable" }, {
            t("mutable struct "), i(0),
            t({ "", "end" })
        }),
        s({ trig = "?", name = "ternary", dscr = "this is just so I remember which way around they go" }, {
            t("? "), i(1, "true condition"), t(" : "), i(2, "false condition")
        }),
    }, {
        s({ trig = "([^%s]*)@ref", name = "julia docstring reference", dscr = "Link to docstrings", regTrig = true, snippetType = "autosnippet" }, {
            t({ "[`" }), f(function(_, par) return par.snippet.captures[1] end, {}), t({ "`](@ref" }), i(1), t({ ")" }), i(0),
        }),
    }
