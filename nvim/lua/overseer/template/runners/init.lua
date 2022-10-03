return {
    generator = function(_,cb)
        local filerunners = {
            julia = function() return { "julia", vim.fn.expand("%:p") } end,
            go = function() return { "go", "run", vim.fn.expand("%:p") } end,
            sh = function() return { "sh", vim.fn.expand("%:p") } end,
            bash = function() return { "bash", vim.fn.expand("%:p") } end,
            fish = function() return { "fish", vim.fn.expand("%:p") } end,
            nu = function() return { "nu", vim.fn.expand("%:p") } end,
            python = function() return { "python", vim.fn.expand("%:p") } end,
            r = function() return { "Rscript", vim.fn.expand("%:p") } end,
            haskell = function() return { "ghci", vim.fn.expand("%:p") } end,
            rust = function() return { "rustc", vim.fn.expand("%:p") } end,
            lua = function() return { "lua", vim.fn.expand("%:p") } end,
            perl = function() return { "perl", vim.fn.expand("%:p") } end,
            ruby = function() return { "ruby", vim.fn.expand("%:p") } end,
            javascript = function() return { "node", vim.fn.expand("%:p") } end,
            html = function() return { "browser", vim.fn.expand("%:p") } end,
            c = function() return "cd " .. vim.fn.expand("%:p:h") .. "&& gcc " .. vim.fn.expand("%:p") .. " -o " .. vim.fn.expand("%:p:r") .. " && " .. vim.fn.expand("%:p:r") end,
            cpp = function() return "cd " .. vim.fn.expand("%:p:h") .. "&& g++ " .. vim.fn.expand("%:p") .. " -o " .. vim.fn.expand("%:p:r") .. " && " .. vim.fn.expand("%:p:r") end,
            fortran = function() return "cd " .. vim.fn.expand("%:p:h") .. "&& gfortran " .. vim.fn.expand("%:p") .. " -o " .. vim.fn.expand("%:p:r") .. " && " .. vim.fn.expand("%:p:r") end,
            asm = function() return "cd " .. vim.fn.expand("%:p:h") .. "&& gcc " .. vim.fn.expand("%:p") .. " -no-pie -o " .. vim.fn.expand("%:p:r") .. " && " .. vim.fn.expand("%:p:r") end,
            java = function() return { "java", vim.fn.expand("%:p") } end,
        }

        local ft = vim.bo.filetype
        cb({ {
            name = "Run " .. ft .. " file (" .. vim.fn.expand("%:t") .. ")",
            builder = function()
                local cmd = filerunners[ft]()
                return {
                    cmd = cmd,
                    name = "Running " .. vim.fn.expand("%:t:r"),
                    components = { "default", "unique" }
                }
            end,
            priority = 4,
            condition = {
                filetype = vim.tbl_keys(filerunners)
            },
        } })
    end,
}
