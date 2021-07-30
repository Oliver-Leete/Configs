require("which-key").register({
    ["<leader>"] = {
        r = {
            w = {"<cmd>%!par w80<cr>", "Wrap File to 80 Characters"}
        }
    },
	["<localleader>"] = {
		h = { "<cmd>Telescope heading<cr>", "Headings" },
		p = { "<plug>MarkdownPreviewToggle", "Preview File" },
	},
}, {
	buffer = 0,
})
require("which-key").register({
    ["<leader>"] = {
        r = {
            w = {"!par w80<cr>", "Wrap Selction to 80 Characters"}
        },
    },
}, {
    mode = "v",
	buffer = 0,
})
