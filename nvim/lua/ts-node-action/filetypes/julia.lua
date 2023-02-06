local ret_san = function(input)
    if type(input) == "string" then return vim.fn.split(input, "\n") end
    local ret = {}
    for _, text in pairs(input) do
        vim.list_extend(ret, vim.fn.split(text, "\n"))
    end
    return ret
end

local padding = {
    [","] = "%s ",
    [";"] = "%s ",
    -- ["{"] = "%s ",
    -- ["}"] = " %s",
    -- ["="] = " %s ",
    -- ["or"] = " %s ",
    -- ["and"] = " %s ",
    -- ["+"] = " %s ",
    -- ["-"] = " %s ",
    -- ["*"] = " %s ",
    -- ["/"] = " %s ",
}

local operators = {
    ["!="] = "==",
    ["=="] = "!=",
    [">"] = "<",
    ["<"] = ">",
    [">="] = "<=",
    ["<="] = ">=",
}

local actions = require("ts-node-action.actions")
local cycle_case       = actions.cycle_case()
local toggle_multiline = actions.toggle_multiline(padding)
local toggle_operator = actions.toggle_operator(operators)
local helpers          = require("ts-node-action.helpers")

local toggle_julia_boolean = function(node)
    return tostring(helpers.node_text(node) ~= "true")
end

local julia_make_begin = function(list, prefix, suffix)
    prefix = prefix or ""
    suffix = suffix or ""
    local ret = { prefix .. "begin" }
    vim.tbl_map(function(text) table.insert(ret, text) end, list)
    table.insert(ret, "end" .. suffix)
    return ret
end

local julia_trim_begin = function(node)
    if node:type() ~= "compound_statement" then return { helpers.node_text(node) } end

    local ret = {}
    for n, _ in node:iter_children() do
        local t = helpers.node_text(n)
        if t ~= "begin" and t ~= "end" and t ~= "" then
            table.insert(ret, t)
        end
    end

    return ret
end

local julia_if_tern = function(node)
    local condition
    local body = {}
    local alternate = {}
    for child, field in node:iter_children() do
        if child:type() == "elseif_clause" then return vim.fn.split(helpers.node_text(node), "\n") end

        if field == "condition" then
            condition = helpers.node_text(child)
        elseif child:type() == "else_clause" then
            for alt_child, _ in child:iter_children() do
                if alt_child:type() ~= "else" then table.insert(alternate, helpers.node_text(alt_child)) end
            end
        elseif child:type() == "end" then
        elseif child:type() == "if" then
        else
            table.insert(body, helpers.node_text(child))
        end

    end
    body = vim.tbl_filter(function(x) return x ~= "" end, body)
    alternate = vim.tbl_filter(function(x) return x ~= "" end, alternate)
    if body[1] == nil or condition == nil then return vim.fn.split(helpers.node_text(node), "\n") end

    local ret
    if alternate[1] == nil then
        local op = " && "
        if condition:sub(1, 1) == "!" then
            condition = condition:sub(2, #condition)
            op = " || "
        end
        if #body > 1 then
            ret = julia_make_begin(body, condition .. op)
        else
            ret = condition .. op .. body[1]
        end
    else
        if #body > 1 and #alternate > 1 then
            ret = julia_make_begin(body, condition .. " ? ", " : begin")
            ret = vim.list_extend(ret, julia_make_begin(alternate), 2)
        elseif #body > 1 then
            ret = julia_make_begin(body, condition .. " ? ", " : " .. alternate[1])
        elseif #alternate > 1 then
            ret = julia_make_begin(alternate, condition .. " ? " .. body[1] .. " : ")
        else
            ret = condition .. " ? " .. body[1] .. " : " .. alternate[1]
        end
    end
    return ret_san(ret), { format = true }
end

local julia_tern_if = function(node)
    local ret = {}
    table.insert(ret, "if " .. helpers.node_text(node:child(0)))
    vim.list_extend(ret, julia_trim_begin(node:child(2)))
    table.insert(ret, "else")
    vim.list_extend(ret, julia_trim_begin(node:child(4)))
    table.insert(ret, "end")
    for i, text in pairs(ret) do
        if text == "else" or text == "end" then
            ret[i] = text
        elseif string.sub(text, 1, 3) == "if " then
            ret[i] = text
        else
            ret[i] = text
        end
    end

    return ret_san(ret), { format = true }
end

local julia_func_short = function(node)
    local body = {}
    local name
    local params
    local where = ""
    for child, field in node:iter_children() do
        local text = helpers.node_text(child)
        if field == "name" then
            name = text
        elseif field == "parameters" then
            params = text
        elseif child:type() == "where_clause" then
            where = text
        elseif text == "function" or text == "end" then
        elseif text:sub(1, 7) == "return " then
            table.insert(body, text:sub(8, #text))
        else
            table.insert(body, text)
        end
    end
    body = vim.tbl_filter(function(x) return x ~= "" end, body)
    if where ~= "" then where = " " .. where end
    local ret
    if name == nil then
        if where ~= "" then return vim.fn.split(helpers.node_text(node), "\n") end
        if #body > 1 then
            if body[#body-1]:sub(1, 7) == "return " then body[#body-1] = body[1]:sub(8, #body[#body-1]) end
            ret = julia_make_begin(body, params .. " -> ")
        else
            if body[1]:sub(1, 7) == "return " then body[1] = body[1]:sub(8, #body[1]) end
            ret = params .. " -> " .. body[1]
        end
    elseif #body > 1 then
        ret = julia_make_begin(body, name .. params .. where .. " = ")
    else
        ret = name .. params .. where .. " = " .. body[1]
    end
    return ret_san(ret), { format = true }
end

local julia_func_long = function(node)
    local body = {}
    local name
    local params
    local where = ""
    for child, field in node:iter_children() do
        local text = helpers.node_text(child)
        if field == "name" then
            name = text
        elseif field == "parameters" then
            params = text
        elseif child:type() == "where_clause" then
            where = text
        elseif text == "=" then
        else
            vim.list_extend(body, julia_trim_begin(child))
        end
    end
    body = vim.tbl_filter(function(x) return x ~= "" end, body)
    if where ~= "" then where = " " .. where end
    local ret = {}
    table.insert(ret, "function " .. name .. params .. where)
    if body[#body]:sub(1, 7) ~= "return " then
        body[#body] = "return " .. body[#body]
    end
    vim.list_extend(ret, body)
    table.insert(ret, "end")
    return ret_san(ret), { format = true }
end

local func_exp = function(node)
    local ret = {}
    local params = helpers.node_text(node:child(0))
    if params:sub(1, 1) ~= "(" then params = "(" .. params .. ")" end
    table.insert(ret, "function" .. params)
    vim.list_extend(ret, julia_trim_begin(node:child(2)))
    table.insert(ret, "end")
    ret[#ret - 1] = "return " .. ret[#ret - 1]
    for i, text in pairs(ret) do
        if i ~= 1 and i ~= #ret then
            ret[i] = text
        end
    end

    return ret_san(ret), { format = true }
end

local function collapse_child_nodes(padding)
    return function(node)
        local replacement = {}

        for child, _ in node:iter_children() do
            table.insert(replacement, helpers.padded_node_text(child, padding))
        end
        replacement = vim.tbl_filter(function(x) return x ~= "" end, replacement)
        table.remove(replacement, 1)
        table.remove(replacement, #replacement)

        return "[" .. table.concat(replacement, "; ") .. "]"
    end
end

local function expand_child_nodes(node)
    local replacement = {}

    for child in node:iter_children() do
        if child:named() then
            table.insert(replacement, child)
        else
            if child:next_sibling() and child:prev_sibling() then
                replacement[#replacement] = replacement[#replacement] .. helpers.node_text(child)
            elseif not child:prev_sibling() then -- Opening brace
                table.insert(replacement, helpers.node_text(child))
            else -- Closing brace
                table.insert(replacement, child)
            end
        end
    end

    return replacement
end

local matrix_toggle_multiline = function(node)
    local fn
    if helpers.multiline_node(node) then
        fn = expand_child_nodes
    else
        fn = collapse_child_nodes(padding)
    end
    return fn(node), {format = true}
end

local begin_to_par = function(node)
    local ret = {}
    for child, _ in node:iter_children() do
        table.insert(ret, helpers.node_text(child))
    end
    ret = vim.tbl_filter(function(x) return x ~= "" and x ~= "begin" and x ~= "end" end, ret)
    return ret_san("(" .. table.concat(ret, "; ") .. ")"), {format = true}
end

local par_to_begin = function(node)
    local ret = {}
    for child, _ in node:iter_children() do
        table.insert(ret, helpers.node_text(child))
    end
    ret = vim.tbl_filter(function(x) return x ~= "" and x ~= "(" and x ~= ")" and x ~= ";" end, ret)
    return ret_san(julia_make_begin(ret)), {format = true}
end

local to_first_arg = function(node, do_n)
    local identifier
    local args = {}

    local params
    local body = {}
    for n, _ in do_n:iter_children() do
        if n:type() == "parameter_list" then
            params = helpers.node_text(n)
        else
            table.insert(body, helpers.node_text(n))
        end
    end
    body = vim.tbl_filter(function(x) return x ~= "" end, body)
    local small_body
    if #body > 3 then
        body[1] = "begin"
        if body[#body - 1]:sub(1, 7) == "return " then
            body[#body - 1] = body[#body - 1]:sub(8, #body[#body - 1])
        end
        small_body = table.concat(body, "\n")
    else
        small_body = body[2]
    end
    table.insert(args, "(" .. params .. ") -> " .. small_body)

    for n, _ in node:iter_children() do
        if n:type() == "identifier" then
            identifier = helpers.node_text(n)
        elseif n:type() == "argument_list" then
            for arg, _ in n:iter_children() do
                table.insert(args, helpers.node_text(arg))
            end
        end
    end
    args = vim.tbl_filter(function(x) return x ~= "" and x ~= "(" and x ~= ")" and x ~= "," end, args)

    return ret_san(identifier .. "(" .. table.concat(args, ", ") .. ")")
end

local toggle_do = function(node)
    for n, _ in node:iter_children() do
        if n:type() == "do_clause" then
            return to_first_arg(node, n)
        end
    end
    return vim.fn.split(helpers.node_text(node), "\n")
end

local to_do_clause = function(node)
    local func
    local str = "("
    local i = 0
    for n, _ in node:iter_children() do
        if i == 1 then
            func = n
        elseif i > 2 then
            str = str .. helpers.node_text(n)
        end
        i = i + 1
    end

    local do_str = " do "
    local ex = {}
    if node:child(1):type() == "function_expression" then
        local params = helpers.node_text(func:child(0))
        if func:child(0):type() == "parameter_list" then
            params = params:sub(2, #params - 1)
        end
        i = 0
        for c, _ in func:iter_children() do
            local text = helpers.node_text(c)
            if i > 0 and text ~= "->" then
                table.insert(ex, text)
            end
            i = i + 1
        end
        do_str = do_str .. params
    elseif node:child(1):type() == "function_definition" then
        local params = helpers.node_text(func:child(1))
        params = params:sub(2, #params - 1)
        i = 0
        for c, _ in func:iter_children() do
            local text = helpers.node_text(c)
            if i > 1 and text ~= "end" then
                table.insert(ex, text)
            end
            i = i + 1
        end
        do_str = do_str .. params
    end
    local ret = { str .. do_str }
    vim.list_extend(ret, ex)
    table.insert(ret, "end")
    return ret
end

local argument_list = function(node)
    if (node:child(1):type() == "function_expression" or node:child(1):type() == "function_definition") then
        return ret_san(to_do_clause(node)), { format = true }
    else
        return toggle_multiline(node), { format = true }
    end
end

return {
    vector_expression = toggle_multiline,
    matrix_expression = {{ matrix_toggle_multiline, name = "Toggle Matrix Multiline"}},
    argument_list = {{ argument_list, name = "Toggle argument list multiline" }},
    parameter_list = toggle_multiline,
    tuple_expression = toggle_multiline,
    boolean_literal = {{ toggle_julia_boolean, name = "Toggle bool" }},
    identifier = cycle_case,
    operator = toggle_operator,
    if_statement = {{ julia_if_tern, name = "Change to ternary expression" }},
    ternary_expression = {{ julia_tern_if, name = "Change to if expression" }},
    function_definition = {{ julia_func_short, name = "Change to short function definition" }},
    short_function_definition = {{ julia_func_long, name = "Change to long function definition" }},
    function_expression = {{ func_exp, name = "Toggle anonymous function " }},
    compound_statement = {{ begin_to_par, name = "Change to parenthesized expression" }},
    parenthesized_expression = {{ par_to_begin, name = "Change to compound statement" }},
    call_expression = {{ toggle_do, name = "Toggle do block" }},
    do_clause = {{ toggle_do, name = "Toggle do block" }},
}
