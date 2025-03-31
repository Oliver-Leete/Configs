vim.keymap.set("n", "<localleader><localleader>", function()
    vim.cmd("TexlabForward")
    vim.cmd("sleep 20m")
    vim.cmd("silent !xdotool key Escape")
    vim.cmd("sleep 200m")
    vim.cmd("silent !xdotool key super+n")
end, { buffer = 0, silent = true })

vim.keymap.set("n", "KK", "<cmd>VimtexDocPackage<cr>", { buffer = 0 })

vim.keymap.set({ "x", "o" }, "am", "<plug>(vimtex-a$)", { buffer = 0, remap = true })
vim.keymap.set({ "x", "o" }, "im", "<plug>(vimtex-i$)", { buffer = 0, remap = true })
vim.keymap.set({ "n", "x", "o" }, "[m", "<cmd>let g:dirJumps='n'<cr>m`<plug>(vimtex-[n)zz", { buffer = 0 })
vim.keymap.set({ "x", "o" }, "alm", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-[l)', '(vimtex-a$)')<cr>",
    { buffer = 0 })
vim.keymap.set({ "x", "o" }, "ilm", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-[l)', '(vimtex-i$)')<cr>",
    { buffer = 0 })
vim.keymap.set({ "n", "x", "o" }, "]m", "<cmd>let g:dirJumps='n'<cr>m`<plug>(vimtex-]n)zz", { buffer = 0 })
vim.keymap.set({ "x", "o" }, "anm", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]n)', '(vimtex-a$)')<cr>",
    { buffer = 0 })
vim.keymap.set({ "x", "o" }, "inm", ":<c-u>call v:lua.plug_targets(v:count, '(vimtex-]n)', '(vimtex-i$)')<cr>",
    { buffer = 0 })

vim.keymap.set("n", "<c-leftmouse>", "<cmd>TexlabForward<cr>")

vim.keymap.set("n", "dpe", "<plug>(vimtex-env-delete)", { buffer = 0, remap = true })
vim.keymap.set("n", "dpc", "<plug>(vimtex-cmd-delete)", { buffer = 0, remap = true })
vim.keymap.set("n", "dpd", "<plug>(vimtex-delim-delete)", { buffer = 0, remap = true })
vim.keymap.set("n", "cpe", "<plug>(vimtex-env-change)", { buffer = 0, remap = true })
vim.keymap.set("n", "cpc", "<plug>(vimtex-cmd-change)", { buffer = 0, remap = true })
vim.keymap.set("n", "cpd", "<plug>(vimtex-delim-change)", { buffer = 0, remap = true })

vim.keymap.set("n", "ype", "<plug>(vimtex-env-surround-operator)", { buffer = 0, remap = true })
vim.keymap.set("x", "ype", "<plug>(vimtex-env-surround-visual)", { buffer = 0, remap = true })

vim.keymap.set("n", "<localleader>d", "<plug>(vimtex-delim-add-modifiers)", { buffer = 0, remap = true })
vim.keymap.set("n", "%", "<plug>(vimtex-%)", { buffer = 0, remap = true })

-- vim.keymap.del("n", ",n", { buffer = 0 })
vim.keymap.set("n", ",nm", "<plug>(vimtex-env-toggle-math)", { buffer = 0, remap = true })
vim.keymap.set("n", ",nf", "<plug>(vimtex-cmd-toggle-frac)", { buffer = 0, remap = true })
vim.keymap.set("n", ",nd", "<plug>(vimtex-delim-toggle-modifier)", { buffer = 0, remap = true })

vim.api.nvim_buf_set_option(0, "textwidth", 100)

vim.b[0].upafunc = function()
    local test_results = vim.fn.systemlist([[rg --json '\\input\{]] .. vim.fn.expand("%:r") .. [[(\.tex)?}']])
    for _, result in pairs(test_results) do
        result = vim.json.decode(result)
        if result and result.type == "match" then
            local file = result.data.path.text
            local linenr = result.data.line_number
            local colnr
            if result.data.submatch and result.data.submatch[1] then
                colnr = result.data.submatch[1].start
            else
                colnr = 0
            end
            vim.cmd.edit({ args = { file } })
            vim.api.nvim_win_set_cursor(0, { linenr, colnr + 7 })
        end
    end
end

vim.g.tex_flavor = "latex"
vim.g.vimtex_quickfix_mode = 0
vim.g.vimtex_doc_confirm_single = 0
vim.g.vimtex_view_general_viewer = "zathura --synctex-editor-command='nvr --servername " ..
    vim.v.servername .. " +%{line} %{input}'"
vim.g.vimtex_view_forward_search_on_start = 1
vim.g.vimtex_view_automatic = 0
vim.g.vimtex_compiler_latexmk = {
    ["callback"] = 1,
    ["continuous"] = 0,
    ["executable"] = "latexmk",
    ["hooks"] = {},
    ["options"] = { "-pdf", "-verbose", "-file-line-error", "-synctex=1", "-interaction=nonstopmode" },
}
