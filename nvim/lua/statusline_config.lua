function Statusline()
    local bufnr = vim.api.nvim_get_current_buf()
    local mode = VimMode()[2]
    if not mode then return "" end

    local hl = "%#WinBar" .. mode .. "#"
    local hle = "%#WinBar" .. mode .. "Ends#"

    local prev = ""
    local statusline = hl .. " " .. VimMode()[1] .. " " .. hle .. " "

    -- git
    local gitstats = vim.b.gitsigns_status_dict
    local git = ""
    if gitstats and gitstats.head then
        local branch = gitstats.head
        if branch then
            git = git .. "%#Normal#" .. " " .. branch .. " "
        end
        local added = gitstats.added
        if added and added > 0 then
            git = git .. "%#GitSignsAdd#" .. " " .. tostring(added) .. " "
        end
        local changed = gitstats.changed
        if changed and changed > 0 then
            git = git .. "%#GitSignsChange#" .. " " .. tostring(changed) .. " "
        end
        local removed = gitstats.removed
        if removed and removed > 0 then
            git = git .. "%#GitSignsDelete#" .. " " .. tostring(removed) .. " "
        end
    end
    if git ~= "" then
        if prev ~= "" then
            statusline = statusline .. hle .. " " .. git
        else
            statusline = statusline .. git
        end
        prev = git
    end

    -- diagnostics
    local diag = ""
    local diagstats = vim.diagnostic.get(bufnr)
    if #diagstats > 0 then
        local count = { 0, 0, 0, 0 }
        for _, diagnostic in ipairs(diagstats) do
            count[diagnostic.severity] = count[diagnostic.severity] + 1
        end

        local error = count[vim.diagnostic.severity.ERROR]
        if error > 0 then
            diag = diag .. "%#DiagnosticError#" .. " " .. tostring(error) .. " "
        end
        local warn = count[vim.diagnostic.severity.WARN]
        if warn > 0 then
            diag = diag .. "%#DiagnosticWarn#" .. " " .. tostring(warn) .. " "
        end
        local info = count[vim.diagnostic.severity.INFO]
        if info > 0 then
            diag = diag .. "%#DiagnosticInfo#" .. " " .. tostring(info) .. " "
        end
        local hint = count[vim.diagnostic.severity.HINT]
        if hint > 0 then
            diag = diag .. "%#DiagnosticHint#" .. " " .. tostring(hint) .. " "
        end
    end
    if diag ~= "" then
        if prev ~= "" then
            statusline = statusline .. hle .. " " .. diag
        else
            statusline = statusline .. diag
        end
        prev = diag
    end

    local tasks = ""
    local tasksstats = require("overseer.task_list").list_tasks()
    if #tasksstats > 0 then
        local tasks_by_status = require("overseer.util").tbl_group_by(tasksstats, "status")

        local pending = tasks_by_status["PENDING"]
        if pending then
            tasks = tasks .. "%#OverseerPENDING#" .. " " .. tostring(#pending) .. " "
        end
        local running = tasks_by_status["RUNNING"]
        if running then
            tasks = tasks .. "%#OverseerRUNNING#" .. " " .. tostring(#running) .. " "
        end
        local canceled = tasks_by_status["CANCELED"]
        if canceled then
            tasks = tasks .. "%#OverseerCANCELED#" .. "ﰸ " .. tostring(#canceled) .. " "
        end
        local success = tasks_by_status["SUCCESS"]
        if success then
            tasks = tasks .. "%#OverseerSUCCESS#" .. " " .. tostring(#success) .. " "
        end
        local failure = tasks_by_status["FAILURE"]
        if failure then
            tasks = tasks .. "%#OverseerFAILURE#" .. " " .. tostring(#failure) .. " "
        end

    end
    if tasks ~= "" then
        if prev ~= "" then
            statusline = statusline .. hle .. " " .. tasks
        else
            statusline = statusline .. tasks
        end
        prev = tasks
    end

    local terms = ""
    local termstats = require("toggleterm.terminal").get_all(true)
    if #termstats > 0 then
        terms = "%#WinBarVisualEnds#" .. terms .. " " .. tostring(#termstats) .. " "
        if prev ~= "" then
            statusline = statusline .. hle .. " " .. terms
        else
            statusline = statusline .. terms
        end
        prev = terms
    end

    local dapstats = require("dap").status(bufnr)
    if dapstats ~= "" then
        local dap = "%#DiagnosticError#" .. dapstats .. " "
        if prev ~= "" then
            statusline = statusline .. hle .. " " .. dap
        else
            statusline = statusline .. dap
        end
        prev = dap
    end

    -- midway
    statusline = statusline .. "%="

    statusline = statusline .. hle .. "" .. hl .. " "
    -- location stuff
    statusline = statusline .. "%l:%c  %p%% "

    if vim.lsp.get_active_clients({ bufnr = bufnr }) then
        local clients = {}
        for _, client in pairs(vim.lsp.get_active_clients({ bufnr = bufnr or vim.api.nvim_get_current_buf() })) do
            if client.name ~= "null-ls" then
                clients[#clients + 1] = client.name
            end
        end
        if next(clients) then
            statusline = statusline .. "  " .. table.concat(clients, '   ')
        end
    end

    statusline = statusline .. " "


    return statusline
end

vim.go.statusline = "%{%v:lua.Statusline()%}"
