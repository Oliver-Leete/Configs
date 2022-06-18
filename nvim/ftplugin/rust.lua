Map("n", "<localleader>i", [[<cmd>RustToggleInlayHints<cr>]], { buffer = 0 })
Map("n", "<localleader>m", [[<cmd>RustExpandMacro<cr>]], { buffer = 0 })
Map("n", "<localleader>p", [[<cmd>RustParentModule<cr>]], { buffer = 0 })
Map("n", "<localleader>c", [[<cmd>RustOpenCargo<cr>]], { buffer = 0 })
Map("n", "<localleader>g", [[<cmd>RustViewCrateGraph<cr>]], { buffer = 0 })

Map("n", "<leader>mm", [[<cmd>RustRunnables<cr>]], { buffer = 0 })
Map("n", "<leader>jj", [[<cmd>RustDebuggables<cr>]], { buffer = 0 })
Map(
    "n",
    "<leader>ll",
    [[<cmd>silent !kittyOneShot maketerm "cd ']]
    .. vim.fn.getcwd()
    .. [[';cargo build --release; cargo flamegraph"<cr>]],
    { buffer = 0 }
)
