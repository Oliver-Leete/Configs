#!/usr/bin/env python3
import subprocess
import time

#--- set commands below
screen_command = "/home/oleete/.config/bin/displayctl auto"
#---

def get(cmd): return subprocess.check_output(cmd).decode("utf-8")
# - to count the occurrenc of " connected "
def count_screens(xr): return xr.count(" connected ")
# - to run the connect / disconnect command(s)
def run_command(cmd): subprocess.Popen(["/bin/bash", "-c", cmd])

# first count
xr1 = None

while True:
    time.sleep(5)
    # second count
    xr2 = count_screens(get(["xrandr"]))
    # check if there is a change in the screen state
    if xr2 != xr1:
        print("change")
        if xr2 > xr1:
            run_command(screen_command)
                    zc)
            run_command("notify-send connected")
        elif xr2 < xr1:
            run_command(screen_command)
            run_command("notify-send disconnected")
    # set the second count as initial state for the next loop
    xr1 = xr2
