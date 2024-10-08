xmonad-dir :=  config_local_directory() / "xmonad"
xmobar-dir :=  config_local_directory() / "xmobar" / "xmobar-git"
# Build and reload xmonad
xmonad-build-reload:
    cd {{ xmonad-dir }} && stack install
    xmonad --recompile
    xmonad --restart

xmobar-build-reload:
    cd {{ xmobar-dir }} && stack install
    xmonad --recompile
    xmonad --restart

xmonad-ctl := home_directory() + "/.cabal/bin/xmonadctl-exe"

# Command xmonad to dump the current window stack set to ~/.xsession-errors
xmonad-log-stack: && xmonad-view-log
    {{ xmonad-ctl }} dump-stack

# Command xmonad to dump the full stack set to ~/.xsession-errors
xmonad-log-full-stack: && xmonad-view-log
    {{ xmonad-ctl }} dump-full-stack

# View the xsession-errors log to see the results of the dumped stack
xmonad-view-log:
    tail -n 50 {{ home_directory() }}/.xsession-errors

# Reload kitty with the new settings
reload-kitty:
    kitty @ load-config

# Evaluate the current status of xmopanel and print it to console
print-xmopanel:
    ./bin/xmopanel
