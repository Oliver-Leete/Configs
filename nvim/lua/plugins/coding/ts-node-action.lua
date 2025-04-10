---@module "lazy"
---@type LazySpec
return {
    "CKolkey/ts-node-action",
    dependencies = { "tpope/vim-repeat" },
    opts = function()
        return {
            julia = require("ts-node-action.filetypes.julia")
        }
    end,
    keys = {
        { ",n", function() require("ts-node-action").node_action() end, desc = "Node action" },
    },
}
