local windline = require("windline")
local helper = require("windline.helpers")
local sep = helper.separators
local vim_components = require("windline.components.vim")

local b_components = require("windline.components.basic")
local state = _G.WindLine.state

local lsp_comps = require("windline.components.lsp")
local git_comps = require("windline.components.git")

local hl_list = {
    Black = { "blue", "ActiveBg" },
    White = { "black", "blue" },
    Inactive = { "VertSplit", "InactiveBg" },
    Active = { "ActiveFg", "ActiveBg" },
}
local basic = {}

basic.divider = { b_components.divider, "" }
basic.underline = { b_components.divider, "_" }
basic.file_name_inactive = { b_components.full_file_name, hl_list.Inactive }
basic.line_col_inactive = { b_components.line_col, hl_list.Inactive }
basic.progress_inactive = { b_components.progress, hl_list.Inactive }

basic.vi_mode = {
    name = "vi_mode",
    hl_colors = {
        Normal = { "black", "red" },
        Insert = { "black", "green" },
        Visual = { "black", "yellow" },
        Replace = { "black", "blue_light" },
        Command = { "black", "magenta" },
        NormalBefore = { "red", "ActiveBg" },
        InsertBefore = { "green", "ActiveBg" },
        VisualBefore = { "yellow", "ActiveBg" },
        ReplaceBefore = { "blue_light", "ActiveBg" },
        CommandBefore = { "magenta", "ActiveBg" },
        NormalAfter = { "red", "blue" },
        InsertAfter = { "green", "blue" },
        VisualAfter = { "yellow", "blue" },
        ReplaceAfter = { "blue_light", "blue" },
        CommandAfter = { "magenta", "blue" },
    },
    text = function()
        return {
            { " ", state.mode[2] },
            { state.mode[1] .. " ", state.mode[2] },
            { sep.right_rounded, state.mode[2] .. "After" },
        }
    end,
}

basic.lsp_diagnos = {
    name = "diagnostic",
    hl_colors = {
        red = { "red", "ActiveBg" },
        yellow = { "yellow", "ActiveBg" },
        blue = { "blue", "ActiveBg" },
    },
    width = 90,
    text = function(bufnr)
        if lsp_comps.check_lsp(bufnr) then
            return {
                { lsp_comps.lsp_error({ format = "  %s" }), "red" },
                { lsp_comps.lsp_warning({ format = "  %s" }), "yellow" },
                { lsp_comps.lsp_hint({ format = "  %s" }), "blue" },
                { lsp_comps.lsp_info({ format = "  %s" }), "blue" },
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
        NormalBefore = { "red", "ActiveBg" },
        InsertBefore = { "green", "ActiveBg" },
        VisualBefore = { "yellow", "ActiveBg" },
        ReplaceBefore = { "blue_light", "ActiveBg" },
        CommandBefore = { "magenta", "ActiveBg" },
        NormalAfter = { "red", "blue" },
        InsertAfter = { "green", "blue" },
        VisualAfter = { "yellow", "blue" },
        ReplaceAfter = { "blue_light", "blue" },
        CommandAfter = { "magenta", "blue" },
    },
    text = function(bufnr)
        return {
            { sep.left_rounded, state.mode[2] .. "Before" },
            { lsp_comps.lsp_name(), state.mode[2] },
            { b_components.line_col_lua, state.mode[2] },
            { "", state.mode[2] },
            { b_components.progress_lua, state.mode[2] },
            { " ", state.mode[2] },
        }
    end,
}
basic.git = {
    name = "git",
    width = 90,
    hl_colors = {
        green = { "green", "ActiveBg" },
        red = { "red", "ActiveBg" },
        blue = { "blue", "ActiveBg" },
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
}

local default = {
    filetypes = { "default" },
    active = {
        basic.vi_mode,
        basic.file,
        { vim_components.search_count(), { "black", "blue" } },
        { sep.right_rounded, hl_list.Black },
        basic.lsp_diagnos,
        basic.divider,
        basic.git,
        { git_comps.git_branch({ icon = "  " }), { "green", "ActiveBg" }, 90 },
        { " ", hl_list.Black },
        basic.right,
    },
    inactive = {
        basic.underline,
    },
}

local quickfix = {
    filetypes = { "qf", "Trouble" },
    active = {
        { " ", hl_list.Black },
        { sep.left_rounded, hl_list.Black },
        { "🚦 Quickfix ", { "ActiveBg", "blue" } },
        { sep.right_rounded, hl_list.Black },
        {
            function()
                return vim.fn.getqflist({ title = 0 }).title
            end,
            { "cyan", "InactiveBg" },
        },
        { " Total : %L ", { "cyan", "InactiveBg" } },
        { " ", { "InactiveFg", "InactiveBg" } },
        basic.divider,
    },
    always_active = true,
    show_last_status = true,
}

windline.setup({
    colors_name = function(colors)
        return colors
    end,
    statuslines = {
        default,
        quickfix,
    },
})