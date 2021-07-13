vim.api.nvim_set_buf_option(0, "compe_latex_insert_code", true)

require("which-key").register({
    ["<localleader>"] = {
        t = {"<cmd>Telescope heading<cr>", "Headings"}
    }
}, {buffer=0})
