local windline = require("windline")
local helper = require("windline.helpers")
local sep = helper.separators

local b_components = require("windline.components.basic")
local state = _G.WindLine.state

local lsp_comps = require("windline.components.lsp")
local git_comps = require("windline.components.git")
local git_rev_components = require('windline.components.git_rev')

local theme_colors = require("kanagawa.colors").setup()

local basic = {}

basic.divider = { b_components.divider, "" }
basic.underline = { b_components.divider, "_" }

basic.vi_mode = {
    name = "vi_mode",
    hl_colors = {
        Normal = { "black", "blue" },
        Insert = { "black", "green" },
        Visual = { "black", "magenta" },
        Replace = { "black", "red" },
        Command = { "black", "yellow" },
        NormalEnds = { "blue", "NormalBg" },
        InsertEnds = { "green", "NormalBg" },
        VisualEnds = { "magenta", "NormalBg" },
        ReplaceEnds = { "red", "NormalBg" },
        CommandEnds = { "yellow", "NormalBg" },
    },
    text = function()
        return {
            { sep.left_rounded, state.mode[2] .. "Ends" },
            { " ", state.mode[2] },
            { state.mode[1] .. " ", state.mode[2] },
            { "" },
            { b_components.progress_lua },
            { b_components.line_col_lua },
            { sep.right_rounded, state.mode[2] .. "Ends" },
        }
    end,
}

basic.lsp_diagnos = {
    name = "diagnostic",
    width = 90,
    text = function(bufnr)
        if lsp_comps.check_lsp(bufnr) then
            return {
                { lsp_comps.lsp_error({ format = "  %s" }), "DiagnosticError" },
                { lsp_comps.lsp_warning({ format = "  %s" }), "DiagnosticWarn" },
                { lsp_comps.lsp_hint({ format = "  %s" }), "DiagnosticHint" },
                { lsp_comps.lsp_info({ format = "  %s" }), "DiagnosticInfo" },
            }
        end
        return ""
    end,
}

basic.right = {
    hl_colors = {
        Normal = { "black", "blue" },
        Insert = { "black", "green" },
        Visual = { "black", "magenta" },
        Replace = { "black", "red" },
        Command = { "black", "yellow" },
        NormalEnds = { "blue", "NormalBg" },
        InsertEnds = { "green", "NormalBg" },
        VisualEnds = { "magenta", "NormalBg" },
        ReplaceEnds = { "red", "NormalBg" },
        CommandEnds = { "yellow", "NormalBg" },
    },
    text = function(bufnr)
        local lsp_names = lsp_comps.lsp_name()(bufnr)
        if lsp_names and lsp_names ~= "" then
            return {
                { sep.left_rounded, state.mode[2] .. "Ends" },
                { lsp_names, state.mode[2] },
                { sep.right_rounded, state.mode[2] .. "Ends" },
            }
        end
        return ""
    end,
}

basic.git = {
    name = "git",
    width = 90,
    text = function(bufnr)
        if git_comps.is_git(bufnr) then
            return {
                { " " },
                { git_comps.diff_added({ format = " %s" }), "diffAdded" },
                { git_comps.diff_removed({ format = "  %s" }), "diffRemoved" },
                { git_comps.diff_changed({ format = "  %s" }), "diffChanged" },
                { git_comps.git_branch(), { "white", "NormalBg" } },
                { " " },
                { git_rev_components.git_rev(), { "white", "NormalBg" } },
            }
        end
        return ""
    end,
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

windline.setup({
    colors_name = function(colors)
        colors.NormalBg = theme_colors.bg
        colors.black = theme_colors.bg
        colors.blue = theme_colors.crystalBlue
        colors.blue_light = theme_colors.fujiGray
        colors.green = theme_colors.autumnGreen
        colors.magenta = theme_colors.oniViolet
        colors.red = theme_colors.autumnRed
        colors.yellow = theme_colors.boatYellow2
        return colors
    end,
    statuslines = {
        {
            filetypes = { "default" },
            active = {
                basic.vi_mode,
                basic.lsp_diagnos,
                basic.dap,
                basic.divider,
                { function()
                    local reg = vim.fn.reg_recording()
                    if reg and reg ~= "" then
                        return "Recording @" .. reg
                    end
                end, { "red", "NormalBg" } },
                basic.divider,
                basic.git,
                basic.right,
            },
            inactive = {
                basic.underline,
            },
        },
    },
    tabline = {
        template = {
            select        = { '', { 'TabSelectionFg', 'TabSelectionBg', } },
            select_start  = { sep.left_rounded, { 'TabSelectionBg', 'TabLineFillBg' } },
            select_end    = { sep.right_rounded, { 'TabSelectionBg', 'TabLineFillBg' } },
            select_last   = { sep.right_rounded, { 'TabSelectionBg', 'TabLineFillBg' } },
            normal        = { '', { 'TabLineFg', 'TabLineBg' } },
            normal_start  = { sep.left_rounded, { 'TabLineBg', 'TabLineFillBg' } },
            normal_end    = { sep.right_rounded, { 'TabLineBg', 'TabLineFillBg' } },
            normal_last   = { sep.right_rounded, { 'TabLineBg', 'TabLineFillBg' } },
            normal_select = { sep.right_rounded, { 'TabLineBg', 'TabLineFillBg' } },
        },
    },
})
