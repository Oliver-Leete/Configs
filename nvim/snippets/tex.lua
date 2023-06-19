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
-- local fmt = require("luasnip.extras.fmt").fmt
-- local rep = lse.rep
local m = lse.match

-- local lse = require("luasnip.extras")
-- local l = lse.lambda
-- local r = lse.rep
-- local p = lse.partial
-- local n = lse.nonempty
-- local dl = lse.dynamic_lambda

local in_mathzone = function()
    return vim.fn['vimtex#syntax#in_mathzone']() == 1
end
local in_text = function()
    return not in_mathzone()
end

local function env(name)
    local is_inside = vim.fn['vimtex#env#is_inside'](name)
    return (is_inside[1] > 0 and is_inside[2] > 0)
end
local function tikz()
    return env("tikzpicture")
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
                    t({ "", "\t\t\\draw [arrow] (" }), i(1, "from node"), t(") "), i(3, "branch type"), t(" ("),
                    i(2, "to node"), t(");"), d(4, rec_draw, {}),
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
                    t({ "", "\t\t\\node (" }), i(2, "node name"), t(") ["), i(3, "options"), t("] {"), i(1, "node text"),
                    t("};"), d(4, rec_node, {})
                }),
                sn(nil, {
                    t({ "", "\t\t\\draw [arrow] (" }), i(1, "from node"), t(") "), i(3, "branch type"), t(" ("),
                    i(2, "to node"), t(");"), d(4, rec_node, {}),
                }),
                sn(nil, {
                    t({ "", "", "\t\t\\draw [arrow] (" }), i(1, "from node"), t(") "), i(3, "branch type"), t(" ("),
                    i(2, "to node"), t(");"), d(4, rec_draw, {}),
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

local ref_or_subref = function(num)
    return c(num, { t("\\cref{"), t("\\subref{") } )
end


return {

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
            t({ "", "" }),
            t(" \t\\caption["), m(1, "^(.-)%."), t("]{"), i(1, "caption"), t("}\\label{fig:"), i(2), t({ "}", "" }),
            t({ "", "" }),
            t({ "\\end{figure}" }), i(0)
        }),
        s({ trig = "twofigure", name = "two figures", dscr = "Two figures of equal width" }, {
            t({ "\\setlength{\\figwidth}{\\textwidth}", "" }),
            t({ "\\begin{figure}[htbp]", "" }),
            t({ "\t\\centering", "" }),
            t({ "\t\\twosubfigures%", "" }),
            t({ "\t{" }), i(3, "directory"), t("}{"), i(4, "label"), t("}{"), i(5, "caption"), t({ "}%", "" }),
            t({ "\t{" }), i(6, "directory"), t("}{"), i(7, "label"), t("}{"), i(8, "caption"), t({ "}", "" }),
            t({ "", "" }),
            t("\t\\caption["), m(1, "^(.-)%."), t("]{"), i(1, "caption"), t("}\\label{fig:"), i(2), t({ "}", "" }),
            t({ "", "" }),
            t({ "\\end{figure}" }), i(0)
        }),
        s({ trig = "figure", name = "single figure", dscr = "insert a single figure" }, {
            t({ "\\begin{figure}[htbp]", "" }),
            t({ "\t\\centering", "" }),
            t("\t\\includegraphics[width=1\\textwidth]{"), i(3, "directory"), t({ "}", "" }),
            t({ "", "" }),
            t("\t\\caption["), m(1, "^(.-)%."), t("]{"), i(1, "caption"), t("}\\label{fig:"), i(2), t({ "}", "" }),
            t({ "", "" }),
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
            t({ "", "" }),
            t("\t\\caption["), m(1, "^(.-)%."), t("]{"), i(1, "caption"), t("}\\label{lst:"), i(2), t({ "}", "" }),
            t({ "", "" }),
            t({ "\\end{codeblock}" })
        }),
        s({ trig = "fortran" }, {
            t({ "\\begin{algorithm}[htbp]", "" }),
            t({ "\t\\begin{Fortran}[1]", "" }),
            i(0), t({ "", "" }),
            t({ "\t\\end{Fortran}", "" }),
            t({ "", "" }),
            t("\t\\caption["), m(1, "^(.-)%."), t("]{"), i(1, "caption"), t("}\\label{alg:"), i(2), t({ "}", "" }),
            t({ "", "" }),
            t({ "\\end{algorithm}" })
        }),
        s({ trig = "algorithm" }, {
            t({ "\\begin{algorithm}[htbp]", "" }),
            t({ "\t\\begin{algorithmic}[1]\\setstretch{1.2}", "" }),
            t({ "\t\t\\Procedure{" }), i(3, "name"), t({ "}{" }), i(4, "args"), t({ "}" }),
            i(0), t({ "", "\t\t\\EndProcedure" }),
            t({ "", "" }),
            t({ "\t\\end{algorithmic}", "" }),
            t({ "", "" }),
            t("\t\\caption["), m(1, "^(.-)%."), t("]{"), i(1, "caption"), t("}\\label{alg:"), i(2), t({ "}", "" }),
            t({ "", "" }),
            t({ "\\end{algorithm}" })
        }),
        s({ trig = "table", name = "table", dscr = "my normal table settings" }, {
            t({ "\\begin{table}[htbp]", "" }),
            t({ "\t\\centering", "" }),
            t("\t\\begin{tabular}{"), i(3, "alignment"), t({ "}\\toprule", "" }),
            t("\t\t"), i(3), t({ "  \\\\ \\midrule", "" }),
            d(4, rec_tab, {}),
            t({ "", "\t\\end{tabular}", "" }),
            t({ "", "" }),
            t("\t\\caption["), m(1, "^(.-)%."), t("]{"), i(1, "caption"), t("}\\label{tab:"), i(2), t({ "}", "" }),
            t({ "", "" }),
            t({ "\\end{table}", "" })
        }),
        s({ trig = "env", name = "begin enviroment", dscr = "begin and end an enviroment" }, {
            t("\\begin{"), i(1), t({ "}", "" }),
            t("\t"), i(0),
            t({ "", "\\begin{" }), f(copy, 1), t("}")
        }),
        s({ trig = "equation", name = "equation" }, {
            t("\\begin{equation}\\eqlabel{"), i(1), t({ "}", "" }),
            t("\t"), i(0),
            t({ "", "\\end{equation}" })
        }),
        s({ trig = "subequation", name = "subequation", dscr = "alligned sub equation" }, {
            t("\\begin{subequations}"),
            t("\t\\begin{align}\\eqlabel{"), i(1), t({ "}", "" }),
            t("\t\t"), i(0),
            t({ "", "\t\\end{align}" }),
            t({ "", "\\end{subequations}" })
        }),

        -- NOTE : FLOWCHARTS
        s({ trig = "node", name = "tikz Node", dscr = "Flowchart Node" }, {
            t("\\node ("), i(1, "nodeID"), t(") ["), i(2, "style"), t(", "), i(3, "position"), t("] {"), i(4, "text"),
            t("};")
        }, { condition = tikz }),
    }, {
        s({ trig = "lbl", name = "label", dscr = "Insert a label" }, {
            c(1, { t("\\label{sect:"), t("\\label{ch:"), t("\\label{ap:"), t("\\label{fig:"), t("\\eqlabel{"), t("\\label{tab:"), t("\\label{alg:"), t("\\label{lst:") }),
            i(2), t("}"),
        }),
        s({ trig = " cite", name = "cite", dscr = "cite" }, {
            t({ "~\\cite{" }), i(1), t({ "}" }),
        }),
        -- NOTE : LABEL REFERENCES
        s({ trig = "Fig", name = "figure reference", dscr = "figure reference" }, {
            ref_or_subref(2), t("fig:"), i(1), t({ "}" }),
        }),
        s({ trig = "Sect", name = "section reference", dscr = "section reference" }, {
            t({ "\\cref{sect:" }), i(1), t({ "}" }),
        }),
        s({ trig = "Chap", name = "chapter reference", dscr = "chapter reference" }, {
            t({ "\\cref{ch:" }), i(1), t({ "}" }),
        }),
        s({ trig = "Equat", name = "equation reference", dscr = "equation reference" }, {
            t({ "\\cref{eq:" }), i(1), t({ "}" }),
        }),
        s({ trig = "Algor", name = "algorithm reference", dscr = "algorithm reference" }, {
            t({ "\\cref{alg:" }), i(1), t({ "}" }),
        }),
        s({ trig = "Appen", name = "appendix reference", dscr = "appendix reference" }, {
            t({ "\\cref{ap:" }), i(1), t({ "}" }),
        }),
        s({ trig = "Table", name = "table reference", dscr = "table reference" }, {
            t({ "\\cref{tab:" }), i(1), t({ "}" }),
        }),
        s({ trig = "Listi", name = "listing reference", dscr = "listing reference" }, {
            t({ "\\cref{lst:" }), i(1), t({ "}" }),
        }),
        -- NOTE : Math auto
        s(
            { trig = "<-", name = "gets" }, { t("\\gets"), },
            { show_condition = in_mathzone, condition = in_mathzone }
        ),
        s(
            { trig = "->", name = "to", }, { t("\\to"), },
            { show_condition = in_mathzone, condition = in_mathzone }
        ),
    }, {
        default_priority = 0,
    }
