vim.api.nvim_set_option("errorformat", [[%tRROR:\ %m\ at\ %f:%l,%-G%.%#]])
vim.api.nvim_set_option("makeprg", [[julia\ -e\ \'using\ Pkg;\ Pkg.precompile()\']])
vim.api.nvim_buf_set_option(0, "commentstring", [[#%s]])

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
        r = {
            d = {[[[fyyO<esc><cmd>.!cat ~/.config/nvim/filetype/julia/func_docstring.txt<cr>pdw>>/TODO:<cr>]], "Make Docstring", noremap=false},
        },
        m = {
            m = {[[<cmd>2TermExec cmd="~/.config/nvim/filetype/julia/precompile"<cr>]], "Precompile"},
            t = {[[<cmd>2TermExec cmd="~/.config/nvim/filetype/julia/test"<cr>]], "Test Package"},
            q = {[[<cmd>2TermExec cmd="exit"<cr>]], "Exit Terminal"},
        }
    }
}, {buffer=0})
