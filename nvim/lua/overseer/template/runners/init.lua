return {
    generator = function(_)
        local filerunners = {
            julia = function() return {"julia", vim.fn.expand("%:p")} end,
            go = function() return {"go", "run", vim.fn.expand("%:p")} end,
            sh = function() return {"sh", vim.fn.expand("%:p")} end,
            fish = function() return {"fish", vim.fn.expand("%:p")} end,
            python = function() return {"python", vim.fn.expand("%:p")} end,
            nu = function() return {"nu", vim.fn.expand("%:p")} end,
            haskell = function() return {"ghci", vim.fn.expand("%:p")} end,
            rust = function() return {"rustc", vim.fn.expand("%:p")} end,
            lua = function() return {"lua", vim.fn.expand("%:p")} end,
            perl = function() return {"perl", vim.fn.expand("%:p")} end,
            ruby = function() return {"ruby", vim.fn.expand("%:p")} end,
        }

        return { {
            name = "Run file (" .. vim.fn.expand("%:t:r") .. ")",
            builder = function()
                local cmd = filerunners[vim.bo.filetype]()
                return {
                    cmd = cmd,
                    tskName = "Running " .. vim.fn.expand("%:t:r"),
                    components = { "default", "unique" }
                }
            end,
            priority = 4,
            condition = {
                filetype = vim.tbl_keys(filerunners)
            },
        } }
    end,
}
