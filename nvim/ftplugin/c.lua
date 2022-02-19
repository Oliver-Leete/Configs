mapxName.group(mapxName.buffer, function()
nnoremap("<leader>mm", [[<cmd>silent !kittyOneShot maketerm "cd /home/oleete/Projects/qmk_firmware; sudo util/docker_build.sh splitkb/kyria:colemak:flash"<cr>]], "Compile and flash")
end)
