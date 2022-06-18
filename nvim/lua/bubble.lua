local windline = require("windline")
local helper = require("windline.helpers")
local sep = helper.separators
local vim_components = require("windline.components.vim")

local b_components = require("windline.components.basic")
local state = _G.WindLine.state

local lsp_comps = require("windline.components.lsp")
local git_comps = require("windline.components.git")
local git_rev = require("windline.components.git_rev")

local hl_list = {
    Black = { "blue", "NormalBg" },
    White = { "black", "blue" },
    Active = { "ActiveFg", "NormalBg" },
}
local basic = {}

basic.divider = { b_components.divider, "" }
basic.underline = { b_components.divider, "_" }

basic.vi_mode = {
    name = "vi_mode",
    hl_colors = {
        Normal = { "black", "red" },
        Insert = { "black", "green" },
        Visual = { "black", "yellow" },
        Replace = { "black", "blue_light" },
        Command = { "black", "magenta" },
        NormalBefore = { "red", "NormalBg" },
        InsertBefore = { "green", "NormalBg" },
        VisualBefore = { "yellow", "NormalBg" },
        ReplaceBefore = { "blue_light", "NormalBg" },
        CommandBefore = { "magenta", "NormalBg" },
        NormalAfter = { "red", "blue" },
        InsertAfter = { "green", "blue" },
        VisualAfter = { "yellow", "blue" },
        ReplaceAfter = { "blue_light", "blue" },
        CommandAfter = { "magenta", "blue" },
    },
    text = function()
        return {
            { sep.left_rounded, state.mode[2] .. "Before" },
            { " ", state.mode[2] },
            { state.mode[1] .. " ", state.mode[2] },
            { sep.right_rounded, state.mode[2] .. "After" },
        }
    end,
}

basic.lsp_diagnos = {
    name = "diagnostic",
    hl_colors = {
        red = { "red", "NormalBg" },
        yellow = { "yellow", "NormalBg" },
        blue = { "blue", "NormalBg" },
    },
    width = 90,
    text = function(bufnr)
        if lsp_comps.check_lsp(bufnr) then
            return {
                { lsp_comps.lsp_error({ format = "  %s" }), "red" },
                { lsp_comps.lsp_warning({ format = "  %s" }), "yellow" },
                { lsp_comps.lsp_hint({ format = "  %s" }), "blue" },
                { lsp_comps.lsp_info({ format = "  %s" }), "blue" },
            }
        end
        return ""
    end,
}

basic.file = {
    name = "file",
    hl_colors = {
        default = hl_list.White,
    },
    text = function()
        return {
            { " ", "default" },
            { b_components.cache_file_icon({ default = "" }), "default" },
            { " ", "default" },
            { b_components.cache_file_name("[No Name]", "unique") },
            { b_components.file_modified(" ") },
        }
    end,
}

basic.right = {
    hl_colors = {
        Normal = { "black", "red" },
        Insert = { "black", "green" },
        Visual = { "black", "yellow" },
        Replace = { "black", "blue_light" },
        Command = { "black", "magenta" },
        NormalBefore = { "red", "NormalBg" },
        InsertBefore = { "green", "NormalBg" },
        VisualBefore = { "yellow", "NormalBg" },
        ReplaceBefore = { "blue_light", "NormalBg" },
        CommandBefore = { "magenta", "NormalBg" },
        NormalAfter = { "red", "blue" },
        InsertAfter = { "green", "blue" },
        VisualAfter = { "yellow", "blue" },
        ReplaceAfter = { "blue_light", "blue" },
        CommandAfter = { "magenta", "blue" },
    },
    text = function(_)
        return {
            { sep.left_rounded, state.mode[2] .. "Before" },
            { lsp_comps.lsp_name(), state.mode[2] },
            { b_components.line_col_lua, state.mode[2] },
            { "", state.mode[2] },
            { b_components.progress_lua, state.mode[2] },
            { " ", state.mode[2] },
            { sep.right_rounded, state.mode[2] .. "Before"}
        }
    end,
    click = function()
        vim.cmd("LspInfo")
    end,
}

basic.git = {
    name = "git",
    width = 90,
    hl_colors = {
        green = { "green", "NormalBg" },
        red = { "red", "NormalBg" },
        blue = { "blue", "NormalBg" },
    },
    text = function(bufnr)
        if git_comps.is_git(bufnr) then
            return {
                { " " },
                { git_comps.diff_added({ format = " %s" }), "green" },
                { git_comps.diff_removed({ format = "  %s" }), "red" },
                { git_comps.diff_changed({ format = "  %s" }), "blue" },
            }
        end
        return ""
    end,
    click = function()
        require("telescope.builtin").git_status(require("telescope.themes").get_ivy())
    end
}

basic.dap = {
    name = "dap",
    width = 90,
    hl_colors = {
        red = { "red", "NormalBg" },
    },
    text = function(_)
        return {
            { " " },
            { require("dap").status, "red" },
        }
    end,
}

local default = {
    filetypes = { "default" },
    active = {
        basic.vi_mode,
        basic.file,
        { vim_components.search_count(), { "black", "blue" } },
        { sep.right_rounded, hl_list.Black },
        basic.lsp_diagnos,
        basic.dap,
        { function()
            local reg = vim.fn.reg_recording()
            if reg and reg ~= "" then
                return "Recording @" .. reg
            end
        end, { "red", "NormalBg" }},
        basic.divider,
        basic.git,
        { git_comps.git_branch({ icon = "  " }), { "green", "NormalBg" }, 90 },
        { git_rev.git_rev({ format = " ⇡%s⇣%s", interval = 10000 }), { "green", "NormalBg" } },
        { " ", hl_list.Black },
        basic.right,
    },
    inactive = {
        basic.underline,
    },
}

windline.setup({
    colors_name = function(colors)
        colors.NormalBg = "#1F1F28"
        colors.black = "#1F1F28"
        return colors
    end,
    statuslines = {
        default,
    },
    tabline = {},
})
