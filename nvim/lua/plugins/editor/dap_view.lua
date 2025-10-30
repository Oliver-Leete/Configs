---@module "lazy"
---@type LazySpec
return {
    "igorlfs/nvim-dap-view",
    lazy = true,
    opts = {
        auto_toggle = true,
        winbar = {
            sections = { "watches", "scopes", "exceptions", "breakpoints", "threads", "repl" },
            controls = { enabled = true },
        },
        windows = {
            position = "right",
            terminal = {
                position = "below",
                height = 0.5
            }
        },
    },
    cmd = {
        "DapViewOpen",
        "DapViewClose",
        "DapViewToggle",
        "DapViewWatch",
        "DapViewJump",
        "DapViewShow",
    },
    config = function(_, opts)
        local dap, dv = require("dap"), require("dap-view")
        dv.setup(opts)

        dap.listeners.before.attach["dap-view-config"] = dv.open
        dap.listeners.before.launch["dap-view-config"] = dv.open
        dap.listeners.before.event_terminated["dap-view-config"] = dv.close
        dap.listeners.before.event_exited["dap-view-config"] = dv.close
    end,
}
