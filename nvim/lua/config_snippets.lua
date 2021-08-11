-------------------------------------------------------------------------------------------------------
--                      _   _   ______    ____   __      __  _____   __  __                        --
--                     | \ | | |  ____|  / __ \  \ \    / / |_   _| |  \/  |                       --
--                     |  \| | | |__    | |  | |  \ \  / /    | |   | \  / |                       --
--                     | . ` | |  __|   | |  | |   \ \/ /     | |   | |\/| |                       --
--                     | |\  | | |____  | |__| |    \  /     _| |_  | |  | |                       --
--                     |_| \_| |______|  \____/      \/     |_____| |_|  |_|                       --
--                                                                                                 --
-------------------------------------------------------------------------------------------------------
-- Oliver Leete <oliverleete@gmail.com>                                                            --
-- https://github.com/oliver-leete                                                                 --
-------------------------------------------------------------------------------------------------------

local ls = require("luasnip")
local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node

-- Every unspecified option will be set to the default.
ls.config.set_config({
    history = true,
    -- Update more often, :h events for more info.
    updateevents = "TextChanged,TextChangedI",
    enable_autosnippets = true,
})

-- args is a table, where 1 is the text in Placeholder 1, 2 the text in
-- placeholder 2,...
local function copy(args)
    return args[1]
end

-- stylua: ignore start
local rec_ls
rec_ls = function()
    return sn(
        nil,
        c(1, {
            -- Order is important, sn(...) first would cause infinite loop of expansion.
            t(""),
            sn(nil, { t({ "", "    \\item " }), i(1), d(2, rec_ls, {}) }),
        })
    )
end

local rec_ds
rec_ds = function()
    return sn(
        nil,
        c(1, {
            -- Order is important, sn(...) first would cause infinite loop of expansion.
            t(""),
            sn(nil, {
                t({"", "", ""}),
                t({"    \\item ["}), i(1, "item description"), t({" ---]", ""}),
                t({"", "        "}), i(2), d(3, rec_ds, {}),
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
                t({"", "        \\draw [arrow] ("}), i(1, "from node"), t({") "}), i(3, "branch type"), t({" ("}), i(2, "to node"), t({");"}), d(4, rec_draw, {}),
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
                t({"", "        \\node ("}), i(2, "node name"), t({") ["}), i(3, "options"), t({"] {"}), i(1, "node text"), t({"};"}), d(4, rec_node, {})
            }),
            sn(nil, {
                t({"","", "        \\draw [arrow] ("}), i(1, "from node"), t({") "}), i(3, "branch type"), t({" ("}), i(2, "to node"), t({");"}), d(4, rec_draw, {}),
            }),
        })
    )
end

ls.snippets = {
    lua = {
        s({trig="sn", name="snippet", dscr="The snippet to make snippets"}, {
            t({"s({trig=\""}), i(1), t({"\", name=\""}), i(2), t({"\", dscr=\""}), i(3), t({"\"}, {", ""}),
            t({"    "}), i(0),
            t({"", "}),"})
        })
    },
    tex = {
        s({trig="ls", name="list", dscr="An infinite list of items"}, {
            t({ "\\begin{itemize}", "    \\item " }), i(1), d(2, rec_ls, {}),
            t({ "", "\\end{itemize}" }),
        }),
        s({trig="disc", name="description", dscr="A described infinite list"}, {
            t({"\\begin{description}", "    \\item ["}), i(1, "item description"), t({" ---]", ""}),
            t({"", "        "}), i(2), d(3, rec_ds, {}), t({"", ""}),
            t({"", "\\end{description}"})
        }),
        s({trig="3f", name="three figures", dscr="Three figures, all equal width"}, {
            t({ "\\setlength{\\figwidth}{\\textwidth}", "" }),
            t({ "\\begin{figure}[htbp]", "" }),
            t({ "    \\centering", "" }),
            t({ "    \\threesubfigures%", "" }),
            t({ "    {" }), i(3, "directory"), t({"}{"}), i(4, "label"), t({"}{"}), i(5, "caption"), t({"}", ""}),
            t({ "    {" }), i(6, "directory"), t({"}{"}), i(7, "label"), t({"}{"}), i(8, "caption"), t({"}", ""}),
            t({ "    {" }), i(9, "directory"), t({"}{"}), i(10, "label"), t({"}{"}), i(11, "caption"), t({"}", ""}),
            t({ "    \\caption["}), i(1, "caption"), t({"]{"}), f(copy, 1), t({"}\\label{fig:"}), i(2, "label"), t({"}", ""}),
            t({ "\\end{figure}" }), i(0)
        }),
        s({trig="2f", name="two figures", dscr="Two figures of equal width"}, {
            t({ "\\setlength{\\figwidth}{\\textwidth}", "" }),
            t({ "\\begin{figure}[htbp]", "" }),
            t({ "    \\centering", "" }),
            t({ "    \\twosubfigures%", "" }),
            t({ "    {" }), i(3, "directory"), t({"}{"}), i(4, "label"), t({"}{"}), i(5, "caption"), t({"}", ""}),
            t({ "    {" }), i(6, "directory"), t({"}{"}), i(7, "label"), t({"}{"}), i(8, "caption"), t({"}", ""}),
            t({ "    \\caption["}), i(1, "caption"), t({"]{"}), f(copy, 1), t({"}\\label{fig:"}), i(2, "label"), t({"}", ""}),
            t({ "\\end{figure}" }), i(0)
        }),
        s({trig="fig", name="single figure", dscr="insert a single figure"}, {
			t({"\\begin{figure}[htbp]", ""}),
			t({"  \\centering", ""}),
			t({"  \\includegraphics[width=1\\textwidth]{"}), i(3, "directory"), t({"}", ""}),
			t({"  \\caption["}), i(1, "caption"), t({"]{"}), f(copy, 1), t({"}\\label{tab:"}), i(2), t({"}", ""}),
			t({"\\end{figure}"}), i(0)
        }),
        s({trig="flow", name="flowchart", dscr="makes flowcharts in the style of chapter intro"}, {
            t({"\\begin{center}", "    \\begin{tikzpicture}[node distance=2cm]", "", "        \\node ("}),
            i(2, "node name"), t({") ["}), i(3, "options"), t({"] {"}), i(1, "node text"), t({"};"}), d(4, rec_node, {}),
            t({"", "", "    \\end{tixzpicture}", "\\end{center}"})

        }),
        s({trig="jul"}, {
			t({"{\\large", ""}),
			t({"\\begin{jllisting}[language=julia, style=jlcodestyle]", ""}),
			t({"  "}), i(0), t({"", ""}),
			t({"\\end{jllisting}", ""}),
			t({"}", ""})
        }),
        s({trig="tab", name="table", dscr="my normal table settings"}, {
			t({"\\begin{table}[htbp]", ""}),
			t({"  \\centering", ""}),
			t({"  \\begin{tabular}{"}), i(3, "alignment"), t({"}\\toprule", ""}),
			t({"    "}), i(0), t({"", ""}),
			t({"  \\end{tabular}", ""}),
			t({"  \\caption[${1:caption}]{${1:caption}}\\label{tab:"}), i(2, "caption"), t({"}", ""}),
			t({"\\end{table}", ""})
        }),
        s({trig="env", name="begin enviroment", dscr="begin and end an enviroment"}, {
            t({"\\begin{"}), i(1), t({"}", ""}),
            t({"    "}), i(0),
            t({"", "\\begin{"}), f(copy, 1), t({"}"})
        }),
        s({trig="im", name="Inline Maths", dscr="Start an inline math enviroment"}, {
            t({"\\("}), i(0), t({"\\)"})
        }),
        s({trig="dm", name="Multiline Maths", dscr="Start a multiline math enviroment"}, {
            t({"\\[", "    "}), i(0), t({"", "\\]"})
        }),
        s({trig="//", name="fraction"}, {
            t({"\\frac{"}), i(1), t({"}{"}), i(2), t("}")
        }),
        s({trig=[[%((.*)%)/]], regTrig=true, name="frac keep"}, {
            t({"\\frac{"}), f(function(args)
                return args[1].captures[1]
            end, {}), t({"}{"}), i(1), t("}"),
        }),
        s({trig=[[([^%s]*[^%)%/])/]], regTrig=true, name="frac keep brackets"}, {
            t({"\\frac{"}), f(function(args)
                return args[1].captures[1]
            end, {}), t({"}{"}), i(1), t("}"),
        }),
    },
    julia = {
        s({trig="docs", name="documentation"}, {
            t({'"""', "    "}),
            i(0),
            t({"", '"""'})
        }),
        s({trig="abst", name="abstract"}, {
            t({"abstract type "}), i(0), t({" end"})
        }),
        s({trig="bare", name="baremodule"}, {
            t({"baremodule "}), i(1),
            t({"", "    "}), i(0),
            t({"", "end"})
        }),
        s({trig="beg", name="begin"}, {
            t({"begin", "    "}),
            i(0),
            t({"","end"})
        }),
        s({trig="do", name="do"}, {
            t({"do "}), i(1),
            t({"", "    "}), i(0),
            t({"", "end"})
        }),
        s({trig="for", name="for"}, {
            t({"for "}), i(1), t({" in "}), i(2),
            t({"", "    "}),  i(3),
            t({"", "end"}), i(0)
        }),
        s({trig="fun", name="function"}, {
            t({"function "}), i(1), t({"("}), i(2),
            t({")", "    "}),  i(3),
            t({"", "end"}), i(0)
        }),
        s({trig="fundoc", name="function"}, {
            t({'"""', "    "}),
            f(copy, 1), t({"("}), f(copy, 2), t({")"}),
            t({"", "",""}), i(3, "a short description"),
            t({"", '"""', ""}),
            t({"function "}), i(1), t({"("}), i(2),
            t({")", "    "}),  i(4),
            t({"", "end"}), i(0)
        }),
        s({trig="if", name="if"}, {
            t({"if "}), i(1),
            t({"", "    "}),  i(2),
            t({"", "end"}), i(0)
        }),
        s({trig="let", name="let"}, {
            t({"let "}), i(1),
            t({"", "    "}),  i(0),
            t({"", "end"})
        }),
        s({trig="mac", name="macro"}, {
            t({"macro "}), i(1), t({"("}), i(2),
            t({")", "    "}),  i(0),
            t({"", "end"})
        }),
        s({trig="mod", name="module"}, {
            t({"module "}), i(1),
            t({"", "    "}),  i(0),
            t({"", "end"})
        }),
        s({trig="mut", name="mutable"}, {
            t({"mutable struct "}), i(0),
            t({"", "end"})
        }),
        s({trig="?", name="ternary", dscr="this is just so I remember which way around they go"}, {
            t({"? "}), i(1, "true condition"), t({" : "}), i(2, "false condition")
        })
    }
}
-- stylua: ignore end

require("luasnip/loaders/from_vscode").load()
