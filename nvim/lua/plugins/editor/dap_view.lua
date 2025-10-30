---@module "lazy"
---@type LazySpec
return {
    "igorlfs/nvim-dap-view",
    lazy = true,
    opts = {
        auto_toggle = true,
        winbar = {
            sections = { "watches", "scopes", "exceptions", "breakpoints", "threads", "repl" },
            headers = {
                breakpoints = "[B]reakpoints",
                scopes = "[S]copes",
                exceptions = "[E]xceptions",
                watches = "[W]atches",
                threads = "[T]hreads",
                repl = "[R]EPL",
                console = "[C]onsole",
            },
            controls = { enabled = true },
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
