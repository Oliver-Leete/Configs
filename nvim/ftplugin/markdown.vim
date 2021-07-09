
lua << EOF
require("which-key").register({
    ["<localleader>"] = {
        t = {"<cmd>Telescope heading<cr>", "Headings"}
    }
}, {buffer=0})
EOF
