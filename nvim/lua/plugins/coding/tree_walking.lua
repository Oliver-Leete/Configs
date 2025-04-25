---@module "lazy"
---@type LazySpec
return {
    -- TODO: Add symbol swapping
    -- TODO: Make JKHL auto enter visual mode
    {
        "gsuuon/tshjkl.nvim",
        opts = {
            select_current_node = true,
            keymaps = {
                toggle = ",z",
                toggle_outer = ",Z",
                toggle_named = "Z"
            },
            marks = {
                parent = {
                    virt_text = { { "h", "ModeMsg" } },
                    virt_text_pos = "overlay",
                },
                child = {
                    virt_text = { { "l", "ModeMsg" } },
                    virt_text_pos = "overlay",
                },
                prev = {
                    virt_text = { { "k", "ModeMsg" } },
                    virt_text_pos = "overlay",
                },
                next = {
                    virt_text = { { "j", "ModeMsg" } },
                    virt_text_pos = "overlay",
                },
            },
            binds = function(bind, tshjkl)
                bind("<Esc>", function() tshjkl.exit(true) end)

                bind("q", function() tshjkl.exit(true) end)

                bind("t", function() vim.notify(tshjkl.current_node():type()) end)
            end,
        },
    },
}
