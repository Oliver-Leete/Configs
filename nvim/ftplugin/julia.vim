set errorformat=%tRROR:\ %m\ at\ %f:%l,%-G%.%#
set makeprg=julia\ -e\ \'using\ Pkg;\ Pkg.precompile()\'

lua << EOF

local juliaREPL = require("toggleterm.terminal").Terminal:new({
    cmd = "julia",
    count = 3,
})

function _juliaREPL_toggle()
    juliaREPL:toggle()
end

require("which-key").register({
    ["<leader>"] = {
        v = {
            i = {"<cmd>lua _juliaREPL_toggle()<cr>", "Julia Terminal"}
        },
        i = {
            i = {"<cmd>lua _juliaREPL_toggle()<cr>", "Julia Terminal"}
        },
    }
}, {buffer=0})
EOF
