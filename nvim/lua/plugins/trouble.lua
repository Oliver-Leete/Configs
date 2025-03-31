local winbar = function()
    local M = {}

    local trouble = require("trouble")

    local sources = {
        { keymap = "E", action = "diagnostics",          title = "Errors" },
        { keymap = "W", action = "snacks",               title = "Searches" },
        { keymap = "F", action = "snacks_files",         title = "File Searches" },
        { keymap = "L", action = "lsp",                  title = "Lsp" },
        { keymap = "S", action = "lsp_document_symbols", title = "Document Symbols" },
    }

    ---@param bufnr? integer
    M.set_winbar_action_keymaps = function(bufnr)
        if bufnr or state.bufnr then
            for _, value in pairs(winbar_info) do
                vim.keymap.set("n", value.keymap, function()
                    value.action()
                end, { buffer = bufnr or state.bufnr })
            end
        end
    end

    ---@param selected_section SectionType
    local set_winbar_opt = function(selected_section)
        if state.winnr and vim.api.nvim_win_is_valid(state.winnr) then
            local winbar = setup.config.winbar.sections
            local winbar_title = {}

            for _, key in ipairs(winbar) do
                local info = winbar_info[key]

                if info ~= nil then
                    local desc = info.desc .. " [" .. info.keymap .. "]" .. " %*"

                    if selected_section == key then
                        desc = "%#TabLineSel# " .. desc
                    else
                        desc = "%#TabLine# " .. desc
                    end

                    table.insert(winbar_title, desc)
                end
            end

            local value = table.concat(winbar_title, "")

            vim.wo[state.winnr][0].winbar = value
        end
    end

    ---@param selected_section SectionType
    M.show_content = function(selected_section)
        winbar_info[selected_section].action()
    end

    ---@param section_name SectionType
    M.update_winbar = function(section_name)
        if setup.config.winbar.show then
            state.current_section = section_name
            set_winbar_opt(state.current_section)
        end
    end

    return M
end

return {
    "folke/trouble.nvim",
    opts = {
        warn_no_restuls = false,
        open_no_results = true,
        preview = {
            type = "main",
        },
    },
    cmd = "Trouble",
    keys = {
        { "<leader>m", function() require("trouble").toggle("snacks") end, desc = "Toggle trouble" },
    },
    specs = {
        "folke/snacks.nvim",
        opts = function(_, opts)
            return vim.tbl_deep_extend("force", opts or {}, {
                picker = {
                    actions = require("trouble.sources.snacks").actions,
                    win = {
                        input = {
                            keys = {
                                ["<c-L>"] = { "trouble_open", mode = { "n", "i" }, },
                            },
                        },
                    },
                },
            })
        end,
    }
}
