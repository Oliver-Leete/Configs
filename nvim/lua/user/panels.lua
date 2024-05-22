require("trouble").setup({
    modes = {
        quickprev = {
            mode = "qflist",
            focus = "true",
            preview = {
                type = "split",
                relative = "win",
                position = "right",
                size = 0.5,
            },
        },
        lspprev = {
            mode = "lsp",
            focus = "true",
            preview = {
                type = "split",
                relative = "win",
                position = "right",
                size = 0.5,
            },
        },
    },
})

local size = function(_, min, max)
    return math.max(
        math.min(
            math.ceil(
                vim.api.nvim_list_uis()[1].width / 5
            ),
            max),
        min)
end
require("edgy").setup({
    wo = {
        winhighlight = "",
        statuscolumn = "%s",
    },
    animate = {
        enabled = false,
    },
    options = {
        left = { size = function() return size("width", 30, 50) end },
        bottom = { size = function() return size("height", 20, 30) end },
    },
    bottom = {
        { ft = "qf", title = " QuickFix" },
        {
            ft = "NoiceHistory",
            title = " Log",
            open = function() require("user.myfuncs").toggle_noice() end,
        },

    },
    keys = {
        ["<esc>"] = function(win)
            win.view.edgebar:close()
        end,
    },
})
