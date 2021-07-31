----------------------------------------------------------------------------------------------------
--                      _   _   ______    ____   __      __  _____   __  __                       --
--                     | \ | | |  ____|  / __ \  \ \    / / |_   _| |  \/  |                      --
--                     |  \| | | |__    | |  | |  \ \  / /    | |   | \  / |                      --
--                     | . ` | |  __|   | |  | |   \ \/ /     | |   | |\/| |                      --
--                     | |\  | | |____  | |__| |    \  /     _| |_  | |  | |                      --
--                     |_| \_| |______|  \____/      \/     |_____| |_|  |_|                      --
--                                                                                                --
----------------------------------------------------------------------------------------------------
-- Oliver Leete <oliverleete@gmail.com>                                                            --
-- https://github.com/oliver-leete                                                                 --
----------------------------------------------------------------------------------------------------

-- Disable builtins
local disabled_built_ins = {
	"netrw",
	"netrwPlugin",
	"netrwSettings",
	"netrwFileHandlers",
	"gzip",
	"zip",
	"zipPlugin",
	"tar",
	"tarPlugin",
	"getscript",
	"getscriptPlugin",
	"vimball",
	"vimballPlugin",
	"2html_plugin",
	"logipat",
	"rrhelper",
	"spellfile_plugin",
	"matchit",
}

for _, plugin in pairs(disabled_built_ins) do
	vim.g["loaded_" .. plugin] = 1
end

-- -- Auto Sessions
-- require("auto-session").setup({
-- 	log_level = "info",
-- 	auto_session_enable_last_session = false,
-- 	auto_session_enabled = true,
-- 	auto_save_enabled = nil,
-- 	auto_restore_enabled = nil,
-- 	auto_session_suppress_dirs = nil,
-- })

-- Autosave Setup

-- require("autosave").setup({
--     verbosity = 0,
--     enabled = true,
--     execution_message = "AutoSave: saved at " .. vim.fn.strftime("%H:%M:%S"),
--     events = {"InsertLeave", "TextChanged"},
--     conditions = {
--         exists = true,
--         filetype_is_not = {},
--         modifiable = true
--     },
--     write_all_buffers = true,
--     on_off_commands = true,
--     clean_command_line_interval = 2500
-- })


require("surround").setup({})

require("hop").setup({ keys = "tnseriaodhgjplfuwybkvmcxzq" })
require("numb").setup()
require("foldsigns").setup()
require("range-highlight").setup()
require("colorizer").setup({ "*" }, {
	RGB = true,
	RRGGBB = true,
	names = false,
	RRGGBBAA = true,
	rgb_fn = true,
	hsl_fn = true,
	css_fn = false,
	mode = "background",
})
