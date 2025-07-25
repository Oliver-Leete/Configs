---@module "lazy"
---@type LazySpec
return {
    {
        "quarto-dev/quarto-nvim",
        dependencies = {
            "jmbuhr/otter.nvim",
            "akinsho/toggleterm.nvim",
        },
        opts = {
            codeRunner = {
                enabled = true,
                default_method = "iron",
            },
        },
        ft = { "quarto" },
        cmd = {
            "QuartoPreview",
            "QuartoClosePreview",
            "QuartoHelp",
            "QuartoActivate",
            "QuartoDiagnostics",
            "QuartoSend",
            "QuartoSendAbove",
            "QuartoSendBelow",
            "QuartoSendAll",
            "QuartoSendLine",
        },
        keys = {
            {
                "<leader>qq",
                function() require("quarto").quartoPreview({}) end,
                ft = "quarto",
                desc = "Open quarto preview",
            },
            {
                "<leader>qc",
                function() require("quarto").quartoClosePreview() end,
                ft = "quarto",
                desc = "Close quarto preview",
            },
            {
                "<leader>qw",
                function() require("quarto").quartoPreviewNoWatch() end,
                ft = "quarto",
                desc = "Open quarto preview (no watch)",
            },
            {
                "<leader>qu",
                function() require("quarto").quartoUpdatePreview() end,
                ft = "quarto",
                desc = "Update quarto preview",
            },
            { "<leader>rc", function() require("quarto.runner").run_cell() end, ft = "quarto", desc = "run cell" },
            {
                "<leader>rr",
                function() require("quarto.runner").run_range() end,
                ft = "quarto",
                desc = "run visual range",
                mode = "x"
            },
        },
    },
    {
        "jmbuhr/otter.nvim",
        opts = {
            lsp = {
                root_dir = function(_, bufnr)
                    return vim.fs.root(bufnr or 0, {
                        ".git",
                        "_quarto.yml",
                        "package.json",
                    }) or vim.fn.getcwd(0)
                end,
            },
        },
    },
    {
        "stevearc/overseer.nvim",
        ---@module "plugins.editor.overseer"
        ---@type OverseerUserConfig
        opts = {
            extra_templates = {
                quarto = {
                    name = "Quarto",
                    generator = function(_, cb)
                        ---@class Params
                        ---@field render_on_save boolean
                        ---@field open_output boolean

                        local TAG = require("overseer.constants").TAG
                        local quarto_params = {
                            render_on_save = {
                                type = "boolean",
                                name = "Watch for file changes",
                                desc = "Rerender the notebook every time the file changes",
                                default = true,
                            },
                            open_output = {
                                type = "boolean",
                                name = "Show on startup",
                                desc = "Open the task view when it starts",
                                default = true,
                            },
                        }

                        ---Check for `render-on-save: false` in _quarto.yml or the current qmd file
                        ---@param user_preference boolean If the user would actually like it to render on save
                        ---@param root_dir string The directory of the project
                        ---@return boolean # The user's choice, unless the file or project disables it
                        local render_on_save_enabled = function(user_preference, root_dir)
                            if user_preference then
                                local lines
                                if root_dir then
                                    local quarto_config = root_dir .. "/_quarto.yml"
                                    lines = vim.fn.readfile(quarto_config)
                                else
                                    -- assumption: the yaml header is not longer than a generous 500 lines
                                    lines = vim.api.nvim_buf_get_lines(0, 0, 500, false)
                                end
                                local query = "render%-on%-save: false"
                                for _, line in ipairs(lines) do
                                    if line:find(query) then return false end
                                end
                            end
                            return user_preference
                        end

                        ---Build a quarto file preview task
                        ---@param params Params The overseer task parameters
                        ---@param mode
                        ---| "file" Render the current file only
                        ---| "project" Render the entire project
                        ---@return overseer.TaskDefinition
                        local quarto_preview = function(params, mode)
                            -- Find root directory / check if it is a project
                            local buffer_path = vim.fn.expand("%:p")
                            local root_dir = require("quarto.util").root_pattern("_quarto.yml")(buffer_path)
                            local args = {}
                            local name
                            local components = { "default", "unique_replace" }

                            if mode == "file" then
                                name = "Render " .. vim.fn.expand("%:t:r")
                                vim.list_extend(args, { buffer_path })
                            else
                                name = "Render project"
                            end

                            local render_on_save = render_on_save_enabled(params.render_on_save, root_dir)
                            if not render_on_save then
                                name = name .. " (no watch)"
                                vim.list_extend(args, { "--no-watch-inputs" })
                            end

                            if params.open_output then vim.list_extend(components, { "open_output" }) end

                            ---@type overseer.TaskDefinition
                            return {
                                name = name,
                                cmd = { "quarto", "preview" },
                                args = args,
                                components = components,
                            }
                        end

                        ---@type fun(search: overseer.SearchParams): boolean, nil|string
                        local is_quarto_file = function(_)
                            local file_extension = vim.fn.expand("%:e")
                            if not file_extension then return false, "Not in a file. exiting." end
                            local quarto_extensions = { "qmd", "Rmd", "ipynb", "md" }
                            if not vim.list_contains(quarto_extensions, file_extension) then
                                return false, "Not a quarto file, ends in " .. file_extension .. " exiting."
                            end
                            return true
                        end

                        ---@type fun(search: overseer.SearchParams): boolean, nil|string
                        local is_quarto_project = function(_)
                            local root_dir = require("quarto.util").root_pattern("_quarto.yml")(vim.fn.expand("%:p"))
                            return root_dir ~= nil
                        end

                        cb({
                            {
                                name = "Preview file",
                                builder = function(params) return quarto_preview(params, "file") end,
                                params = quarto_params,
                                condition = {
                                    callback = is_quarto_file,
                                },
                                tags = { TAG.BUILD },
                            },
                            {
                                name = "Preview project",
                                builder = function(params) return quarto_preview(params, "project") end,
                                params = quarto_params,
                                condition = {
                                    callback = is_quarto_project,
                                },
                            },
                        })
                    end,
                },
            },
        },
    },
}
