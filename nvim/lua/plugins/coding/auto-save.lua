---@module "lazy"
---@type LazySpec
return {
    "okuuva/auto-save.nvim",
    opts = {
        condition = function(buf)
            return vim.bo[buf].modifiable
                and not vim.list_contains({ "oil", "qf", "OverseerForm" }, vim.bo[buf].filetype)
        end,
    },
}
