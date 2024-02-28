# Build and reload xmonad
xmonad-build-reload:
    cd {{ config_local_directory() }}/xmonad && {{ config_local_directory() }}/bin/xmonadRebuild

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
    pkill -10 kitty

# Evaluate the current status of xmopanel and print it to console
print-xmopanel:
    ./bin/xmopanel
