#!/usr/bin/env python3
import subprocess
import time


def get(cmd):
    """Read the input from some command."""
    return subprocess.check_output(cmd).decode("utf-8")


def count_screens(xr):
    """Count the number of connected screens."""
    return xr.count(" connected ")


def count_nets(xr):
    """Count the number of connected networks."""
    return xr.count(":connected:")


# - to run the command(s)
def run_command(cmd):
    """Run some command."""
    subprocess.Popen(["/bin/bash", "-c", cmd])


screen_command = "/home/oleete/.config/bin/displayctl auto"
net_command = "/home/oleete/.config/bin/statusNotify net"

# first values
screenCount1 = count_screens(get(["xrandr"]))
networkCount1 = count_nets(get(["nmcli", "-t", "device"]))
lidOpen1 = get(["cat", "/proc/acpi/button/lid/LID/state"])

time.sleep(0.5)
run_command(screen_command)
while True:
    time.sleep(1)

    # second values
    screenCount2 = count_screens(get(["xrandr"]))
    networkCount2 = count_nets(get(["nmcli", "-t", "device"]))
    lidOpen2 = get(["cat", "/proc/acpi/button/lid/LID/state"])

    # check if there is a change in the screen state
    if (screenCount2 != screenCount1) or (lidOpen2 != lidOpen1):
        run_command(screen_command)
    if networkCount2 != networkCount1:
        run_command(net_command)

    # set the second values as initial state for the next loop
    screenCount1 = screenCount2
    networkCount1 = networkCount2
    lidOpen1 = lidOpen2
