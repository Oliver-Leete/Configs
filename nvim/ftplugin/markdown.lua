require("which-key").register({
    ["<leader>"] = {
        r = {
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
    },
}, {
    mode = "v",
	buffer = 0,
})
