local size = function(dir, min, max)
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
        bottom = { size = function() return size("height", 20, 40) end },
        top = { size = function() return size("height", 20, 40) end }
    },
    top = {
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
