local ls = require("luasnip")

ls.config.set_config({
    history = false,
    updateevents = "TextChanged,TextChangedI",
})

local s = ls.snippet
local sn = ls.snippet_node
local isn = ls.indent_snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local r = ls.restore_node
local events = require("luasnip.util.events")
local ai = require("luasnip.nodes.absolute_indexer")
local lse = require("luasnip.extras")
local fmt = require("luasnip.extras.fmt").fmt
local rep = lse.rep
local m = lse.match

-- local lse = require("luasnip.extras")
-- local l = lse.lambda
-- local r = lse.rep
-- local p = lse.partial
-- local n = lse.nonempty
-- local dl = lse.dynamic_lambda

-- NOTE : LUA
ls.add_snippets("lua", {
    lua = {
        s({ trig = "sn", name = "snippet", dscr = "The snippet to make snippets" }, {
            t({ "s({trig=\"" }), i(1), t({ "\", name=\"" }), i(2), t({ "\", dscr=\"" }), i(3), t({ "\"}, {", "" }),
            t("\t"), i(0),
            t({ "", "})," })
        })
    },
})

-- NOTE : JULIA

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

ls.add_snippets("julia", {
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
]]       , {
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
]]               ,
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
})

-- NOTE : TEX

local tex = {}
tex.in_mathzone = function()
    return vim.fn['vimtex#syntax#in_mathzone']() == 1
end
tex.in_text = function()
    return not tex.in_mathzone()
end

local function copy(args)
    return args[1]
end

local rec_ls
rec_ls = function()
    return sn(
        nil,
        c(1, {
            t(""),
            sn(nil, { t({ "", "\t\\item " }), i(1), d(2, rec_ls, {}) }),
        })
    )
end
local rec_ds
rec_ds = function()
    return sn(
        nil,
        c(1, {
            t(""),
            sn(nil, {
                t({ "", "", "" }),
                t("\t\\item ["), i(1, "item description"), t({ " ---]", "" }),
                t({ "", "\t\t" }), i(2), d(3, rec_ds, {}),
            }),
        })
    )
end

local rec_draw
rec_draw = function()
    return sn(
        nil,
        c(1, {
            t(""),
            sn(nil, {
                t({ "", "\t\t\\draw [arrow] (" }), i(1, "from node"), t(") "), i(3, "branch type"), t(" ("), i(2, "to node"), t(");"), d(4, rec_draw, {}),
            }),
        })
    )
end
local rec_node
rec_node = function()
    return sn(
        nil,
        c(1, {
            t(""),
            sn(nil, {
                t({ "", "\t\t\\node (" }), i(2, "node name"), t(") ["), i(3, "options"), t("] {"), i(1, "node text"), t("};"), d(4, rec_node, {})
            }),
            sn(nil, {
                t({ "", "\t\t\\draw [arrow] (" }), i(1, "from node"), t(") "), i(3, "branch type"), t(" ("), i(2, "to node"), t(");"), d(4, rec_node, {}),
            }),
            sn(nil, {
                t({ "", "", "\t\t\\draw [arrow] (" }), i(1, "from node"), t(") "), i(3, "branch type"), t(" ("), i(2, "to node"), t(");"), d(4, rec_draw, {}),
            }),
        })
    )
end

local rec_tab
rec_tab = function()
    return sn(
        nil, {
        c(1, {
            sn(nil, { t("\t\t"), i(1), t({ "  \\\\ \\bottomrule", "" }), }),
            sn(nil, { t("\t\t"), i(1), t({ "  \\\\", "" }), d(2, rec_tab, {}) }),
        })
    })
end
ls.add_snippets("tex", {
    s({ trig = "label", name = "label", dscr = "Insert a label" }, {
        t("\\label{"),
        c(1, { t("sect"), t("ch"), t("ap"), t("fig"), t("eq"), t("tab"), t("alg"), t("lst") }),
        t(":"), i(2), t("}"),
    }),
    -- NOTE : LABEL REFERENCES
    s({ trig = "fig", name = "figure reference", dscr = "figure reference" }, {
        t({ "Figure~\\ref{fig:" }), i(1), t({ "}" }),
    }),
    s({ trig = "sect", name = "section reference", dscr = "section reference" }, {
        t({ "Section~\\ref{sect:" }), i(1), t({ "}" }),
    }),
    s({ trig = "ch", name = "chapter reference", dscr = "chapter reference" }, {
        t({ "Chapter~\\ref{ch:" }), i(1), t({ "}" }),
    }),
    s({ trig = "eq", name = "equation reference", dscr = "equation reference" }, {
        t({ "Equation~\\ref{eq:" }), i(1), t({ "}" }),
    }),
    s({ trig = "alg", name = "algorithm reference", dscr = "algorithm reference" }, {
        t({ "Algorithm~\\ref{alg:" }), i(1), t({ "}" }),
    }),
    s({ trig = "ap", name = "appendix reference", dscr = "appendix reference" }, {
        t({ "Appendix~\\ref{ap:" }), i(1), t({ "}" }),
    }),
    s({ trig = "tab", name = "table reference", dscr = "table reference" }, {
        t({ "Table~\\ref{tab:" }), i(1), t({ "}" }),
    }),
    s({ trig = "lst", name = "listing reference", dscr = "listing reference" }, {
        t({ "Listing~\\ref{lst:" }), i(1), t({ "}" }),
    }),

    -- NOTE : ENVIROMENTS
    s({ trig = "ls", name = "list", dscr = "An infinite list of items" }, {
        t({ "\\begin{itemize}", "\t\\item " }), i(1), d(2, rec_ls, {}),
        t({ "", "\\end{itemize}" }),
    }),
    s({ trig = "disc", name = "description", dscr = "A described infinite list" }, {
        t({ "\\begin{description}", "\t\\item [" }), i(1, "item description"), t({ " ---]", "" }),
        t({ "", "\t\t" }), i(2), d(3, rec_ds, {}), t({ "", "" }),
        t({ "", "\\end{description}" })
    }),
    s({ trig = "threefigure", name = "three figures", dscr = "Three figures, all equal width" }, {
        t({ "\\setlength{\\figwidth}{\\textwidth}", "" }),
        t({ "\\begin{figure}[htbp]", "" }),
        t({ "\t\\centering", "" }),
        t({ "\t\\threesubfigures%", "" }),
        t({ "\t{" }), i(3, "directory"), t("}{"), i(4, "label"), t("}{"), i(5, "caption"), t({ "}%", "" }),
        t({ "\t{" }), i(6, "directory"), t("}{"), i(7, "label"), t("}{"), i(8, "caption"), t({ "}%", "" }),
        t({ "\t{" }), i(9, "directory"), t("}{"), i(10, "label"), t("}{"), i(11, "caption"), t({ "}%", "" }),
        t(" \t\\caption["), m(1, "^(.-)%."), t("]{"), i(1, "caption"), t("}\\label{fig:"), i(2), t({ "}", "" }),
        t({ "\\end{figure}" }), i(0)
    }),
    s({ trig = "twofigure", name = "two figures", dscr = "Two figures of equal width" }, {
        t({ "\\setlength{\\figwidth}{\\textwidth}", "" }),
        t({ "\\begin{figure}[htbp]", "" }),
        t({ "\t\\centering", "" }),
        t({ "\t\\twosubfigures%", "" }),
        t({ "\t{" }), i(3, "directory"), t("}{"), i(4, "label"), t("}{"), i(5, "caption"), t({ "}%", "" }),
        t({ "\t{" }), i(6, "directory"), t("}{"), i(7, "label"), t("}{"), i(8, "caption"), t({ "}", "" }),
        t("\t\\caption["), m(1, "^(.-)%."), t("]{"), i(1, "caption"), t("}\\label{fig:"), i(2), t({ "}", "" }),
        t({ "\\end{figure}" }), i(0)
    }),
    s({ trig = "figure", name = "single figure", dscr = "insert a single figure" }, {
        t({ "\\begin{figure}[htbp]", "" }),
        t({ "\t\\centering", "" }),
        t("\t\\includegraphics[width=1\\textwidth]{"), i(3, "directory"), t({ "}", "" }),
        t("\t\\caption["), m(1, "^(.-)%."), t("]{"), i(1, "caption"), t("}\\label{fig:"), i(2), t({ "}", "" }),
        t("\\end{figure}"), i(0)
    }),
    s({ trig = "flow", name = "flowchart", dscr = "makes flowcharts in the style of chapter intro" }, {
        t({ "\\begin{center}", "\t\\begin{tikzpicture}[node distance=2cm]", "", "\t\t\\node (" }),
        i(2, "node name"), t(") ["), i(3, "options"), t("] {"), i(1, "node text"), t("};"), d(4, rec_node, {}),
        t({ "", "", "\t\\end{tixzpicture}", "\\end{center}" })

    }),
    s({ trig = "julia" }, {
        t({ "\\begin{codeblock}[htbp]", "" }),
        t({ "\t{\\large", "" }),
        t({ "\t\t\\begin{jllisting}[language=julia, style=jlcodestyle]", "" }),
        i(0), t({ "", "" }),
        t({ "\t\t\\end{jllisting}", "" }),
        t({ "\t}", "" }),
        t("\t\\caption["), m(1, "^(.-)%."), t("]{"), i(1, "caption"), t("}\\label{lst:"), i(2), t({ "}", "" }),
        t({ "\\end{codeblock}" })
    }),
    s({ trig = "fortran" }, {
        t({ "\\begin{algorithm}[htbp]", "" }),
        t({ "\t\\begin{Fortran}[1]", "" }),
        i(0), t({ "", "" }),
        t({ "\t\\end{Fortran}", "" }),
        t("\t\\caption["), m(1, "^(.-)%."), t("]{"), i(1, "caption"), t("}\\label{alg:"), i(2), t({ "}", "" }),
        t({ "\\end{algorithm}" })
    }),
    s({ trig = "pseudo" }, {
        t({ "\\begin{algorithm}[htbp]", "" }),
        t({ "\t\\begin{algorithmic}[1]", "" }),
        i(0), t({ "", "" }),
        t({ "\t\\end{algorithmic}", "" }),
        t("\t\\caption["), m(1, "^(.-)%."), t("]{"), i(1, "caption"), t("}\\label{alg:"), i(2), t({ "}", "" }),
        t({ "\\end{algorithm}" })
    }),
    s({ trig = "table", name = "table", dscr = "my normal table settings" }, {
        t({ "\\begin{table}[htbp]", "" }),
        t({ "\t\\centering", "" }),
        t("\t\\begin{tabular}{"), i(3, "alignment"), t({ "}\\toprule", "" }),
        t("\t\t"), i(3), t({ "  \\\\ \\midrule", "" }),
        d(4, rec_tab, {}),
        t({ "", "\t\\end{tabular}", "" }),
        t("\t\\caption["), m(1, "^(.-)%."), t("]{"), i(1, "caption"), t("}\\label{tab:"), i(2), t({ "}", "" }),
        t({ "\\end{table}", "" })
    }),
    s({ trig = "env", name = "begin enviroment", dscr = "begin and end an enviroment" }, {
        t("\\begin{"), i(1), t({ "}", "" }),
        t("\t"), i(0),
        t({ "", "\\begin{" }), f(copy, 1), t("}")
    }),
    s({ trig = "im", name = "Inline Maths", dscr = "Start an inline math enviroment" }, {
        t("\\("), i(0), t("\\)")
    }),
    s({ trig = "dm", name = "Multiline Maths", dscr = "Start a multiline math enviroment" }, {
        t({ "\\[", "\t" }), i(0), t({ "", "\\]" })
    }),

    -- NOTE : MATHS
    s({ trig = "%s/", name = "fraction", regTrig = true }, {
        t("\\frac{"), i(1), t("}{"), i(2), t("}")
    },
        { show_condition = tex.in_mathzone, condition = tex.in_mathzone }
    ),
    s({ trig = [[%((.*)%)/]], name = "frac keep", regTrig = true }, {
        t("\\frac{"), f(function(_, par)
            return par.snippet.captures[1]
        end, {}), t("}{"), i(1), t("}"),
    },
        { show_condition = tex.in_mathzone, condition = tex.in_mathzone }
    ),
    s({ trig = [[([^%s]+)/]], name = "frac keep brackets", regTrig = true }, {
        t("\\frac{"), f(function(_, par)
            return par.snippet.captures[1]
        end, {}), t("}{"), i(1), t("}"),
    },
        { show_condition = tex.in_mathzone, condition = tex.in_mathzone }
    ),
    s({ trig = [[%((.*)%)/%((.*)%)]], name = "full frac", regTrig = true }, {
        t("\\frac{"), f(function(_, par)
            return par.snippet.captures[1]
        end, {}), t("}{"),
        f(function(_, par)
            return par.snippet.captures[2]
        end, {}), t("}"), i(0)
    },
        { show_condition = tex.in_mathzone, condition = tex.in_mathzone }
    ),

    -- NOTE : FLOWCHARTS
    s({ trig = "node", name = "tikz Node", dscr = "Flowchart Node" }, {
        t("\\node ("), i(1, "nodeID"), t(") ["), i(2, "style"), t(", "), i(3, "position"), t("] {"), i(4, "text"), t("};")
    }),
})
