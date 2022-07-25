local async = require("neotest.async")
local context_manager = require("plenary.context_manager")
local lib = require("neotest.lib")
local open = context_manager.open
local Path = require("plenary.path")
local with = context_manager.with
local xml = require("neotest.lib.xml")
local xml_tree = require("neotest.lib.xml.tree")

local adapter = { name = "neotest-rust" }

adapter.root = lib.files.match_root_pattern("Project.toml")

function adapter.is_test_file(file_path)
    return vim.endswith(file_path, ".jl")
end

return adapter
