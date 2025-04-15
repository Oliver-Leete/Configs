---@module "lazy"
---@type LazySpec
return {
    "igorlfs/nvim-dap-view",
    lazy = true,
    opts = {},
    config = function(_, opts)
        local dap, dv = require("dap"), require("dap-view")
        dv.setup(opts)

        dap.listeners.before.attach["dap-view-config"] = dv.open
        dap.listeners.before.launch["dap-view-config"] = dv.open
        dap.listeners.before.event_terminated["dap-view-config"] = dv.close
        dap.listeners.before.event_exited["dap-view-config"] = dv.close
    end,
}
