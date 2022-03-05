vim.g.VM_mouse_mappings = true
vim.g.VM_leader="="

vim.cmd([[
    let g:VM_maps = {}
    let g:VM_maps["Select Cursor Down"] = 'C'
    let g:VM_maps["Select Cursor Up"]   = '<m-C>'
]])
