#!/usr/bin/env python3
import subprocess
import time

#--- set commands below
screen_command = "/home/oleete/.config/bin/displayctl auto"
net_command = "/home/oleete/.config/bin/statusNotify net"
lid_open_command = "/home/oleete/.config/bin/displayctl auto"
lid_closed_command = "/home/oleete/.config/bin/displayctl auto"
#---

time.sleep(1)

# read the input from some commands
def get(cmd): return subprocess.check_output(cmd).decode("utf-8")

# - to count the occurrenc of " connected "
def count_screens(xr): return xr.count(" connected ")
def count_nets(xr): return xr.count(":connected:")

# - to run the command(s)
def run_command(cmd): subprocess.Popen(["/bin/bash", "-c", cmd])

# first count
xr1 = count_screens(get(["xrandr"]))
net1 = count_nets(get(["nmcli", "-t", "device"]))
lid1 = get(["cat", "/proc/acpi/button/lid/LID/state"])

run_command(screen_command)

while True:
    time.sleep(1)
    # second count
    xr2 = count_screens(get(["xrandr"]))
    net2 = count_nets(get(["nmcli", "-t", "device"]))
    lid2 = get(["cat", "/proc/acpi/button/lid/LID/state"])
    # check if there is a change in the screen state
    if (xr2 != xr1) or (lid2 != lid1):
        print("Displays Change")
        run_command(screen_command)
    if net2 != net1:
        print("Net Change")
        run_command(net_command)
    # set the second count as initial state for the next loop
    xr1 = xr2
    net1 = net2
    lid1 = lid2

# #!/usr/bin/env python3
# import subprocess
# import time

# #--- set commands below
# screen_command = "/home/oleete/.config/bin/displayctl auto"
# net_command = "/home/oleete/.config/bin/statusNotify net"
# #---

# def get(cmd): return subprocess.check_output(cmd).decode("utf-8")

# # - to count the occurrenc of " connected "
# def count_nets(xr): return xr.count(":connected:")

# # - to run the connect / disconnect command(s)
# def run_command(cmd): subprocess.Popen(["/bin/bash", "-c", cmd])

# # first count
# xr1 = get(["displayctl", "fingOnly"])
# net1 = count_nets(get(["nmcli", "-t", "device"]))

# run_command(screen_command)

# while True:
#     time.sleep(1)
#     # second count
#     xr2 = get(["displayctl", "fingOnly"])
#     net2 = count_nets(get(["nmcli", "-t", "device"]))

#     # check if there is a change in the state
#     if xr2 != xr1:
#         print("Displays Change")
#         run_command(screen_command)
#     if net2 != net1:
#         print("Net Change")
#         run_command(net_command)
#     # set the second count as initial state for the next loop
#     xr1 = xr2
#     net1 = net2
