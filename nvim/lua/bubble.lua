local windline = require("windline")
local helper = require("windline.helpers")
local sep = helper.separators
local vim_components = require("windline.components.vim")

local b_components = require("windline.components.basic")
local state = _G.WindLine.state

local lsp_comps = require("windline.components.lsp")
local git_comps = require("windline.components.git")
local git_rev = require("windline.components.git_rev")

local theme_colors = require("kanagawa.colors").setup()

local hl_list = {
    Black = { "blue_light", "NormalBg" },
    White = { "black", "blue_light" },
    Active = { "ActiveFg", "NormalBg" },
}
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
        NormalBefore = { "blue", "NormalBg" },
        InsertBefore = { "green", "NormalBg" },
        VisualBefore = { "magenta", "NormalBg" },
        ReplaceBefore = { "red", "NormalBg" },
        CommandBefore = { "yellow", "NormalBg" },
        NormalAfter = { "blue", "blue_light" },
        InsertAfter = { "green", "blue_light" },
        VisualAfter = { "magenta", "blue_light" },
        ReplaceAfter = { "red", "blue_light" },
        CommandAfter = { "yellow", "blue_light" },
    },
    text = function()
        local bufnr = vim.api.nvim_get_current_buf()
        local coltype
        if Is_special(bufnr) and not SpecialName(bufnr) then
            coltype = "Before"
        else
            coltype = "After"
        end
        return {
            { sep.left_rounded, state.mode[2] .. "Before" },
            { " ", state.mode[2] },
            { state.mode[1] .. " ", state.mode[2] },
            { sep.right_rounded, state.mode[2] .. coltype },
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

basic.file = {
    name = "file",
    hl_colors = {
        default = hl_list.White,
    },
    text = function()
        local bufnr = vim.api.nvim_get_current_buf()
        if Is_special(bufnr) then
            local specialname = SpecialName(bufnr)
            if specialname then
                return {
                    { " ", "default" },
                    { SpecialName(bufnr) },
                    { " ", "default" },
                    { vim_components.search_count(), { "black", "blue_light" } },
                    { sep.right_rounded, hl_list.Black },
                }
            else
                return {
                    { " ", hl_list.Black },
                }
            end
        else
            return {
                { " ", "default" },
                { b_components.cache_file_icon({ default = "" }), "default" },
                { " ", "default" },
                { b_components.cache_file_name("[No Name]", "unique") },
                { b_components.file_modified(" ") },
                { vim_components.search_count(), { "black", "blue_light" } },
                { sep.right_rounded, hl_list.Black },
            }
        end
    end,
}

basic.right = {
    hl_colors = {
        Normal = { "black", "blue" },
        Insert = { "black", "green" },
        Visual = { "black", "magenta" },
        Replace = { "black", "red" },
        Command = { "black", "yellow" },
        NormalBefore = { "blue", "NormalBg" },
        InsertBefore = { "green", "NormalBg" },
        VisualBefore = { "magenta", "NormalBg" },
        ReplaceBefore = { "red", "NormalBg" },
        CommandBefore = { "yellow", "NormalBg" },
        NormalAfter = { "blue", "NormalBg" },
        InsertAfter = { "green", "NormalBg" },
        VisualAfter = { "magenta", "NormalBg" },
        ReplaceAfter = { "red", "NormalBg" },
        CommandAfter = { "yellow", "NormalBg" },
    },
    text = function(_)
        return {
            { sep.left_rounded, state.mode[2] .. "Before" },
            { lsp_comps.lsp_name(), state.mode[2] },
            { b_components.line_col_lua, state.mode[2] },
            { "", state.mode[2] },
            { b_components.progress_lua, state.mode[2] },
            { " ", state.mode[2] },
            { sep.right_rounded, state.mode[2] .. "After" }
        }
    end,
    click = function()
        vim.cmd("LspInfo")
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
                basic.file,
                basic.lsp_diagnos,
                basic.dap,
                { function()
                    local reg = vim.fn.reg_recording()
                    if reg and reg ~= "" then
                        return "Recording @" .. reg
                    end
                end, { "red", "NormalBg" } },
                basic.divider,
                basic.git,
                { git_comps.git_branch({ icon = "  " }), "normal", 90 },
                { git_rev.git_rev({ format = " ⇡%s⇣%s", interval = 10000 }), { "green", "NormalBg" } },
                { " ", hl_list.Black },
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
