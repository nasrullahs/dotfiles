#!/usr/bin/env python3
#
# Copyright (C) 2016 James Murphy
# Licensed under the GPL version 2 only
#
# A battery indicator blocklet script for i3blocks

from subprocess import check_output

status = check_output(['acpi'], universal_newlines=True)

if not status:
    # stands for no battery found
    fulltext = "<span color='red'><span font='FontAwesome'>\uf00d \uf240</span></span>"
    percentleft = 100
else:
    # if there is more than one battery in one laptop, the percentage left is
    # available for each battery separately, although state and remaining
    # time for overall block is shown in the status of the first battery
    batteries = status.split("\n")
    state_batteries=[]
    commasplitstatus_batteries=[]
    percentleft_batteries=[]
    for battery in batteries:
        if battery!='':
            state_batteries.append(battery.split(": ")[1].split(", ")[0])
            commasplitstatus = battery.split(", ")
            percentleft_batteries.append(int(commasplitstatus[1].rstrip("%\n")))
            commasplitstatus_batteries.append(commasplitstatus)
    state = state_batteries[0]
    commasplitstatus = commasplitstatus_batteries[0]
    percentleft = int(sum(percentleft_batteries)/len(percentleft_batteries))
    # stands for charging
    FA_LIGHTNING = "<span color='yellow'><span font='FontAwesome'>\uf0e7</span></span>"

    # stands for plugged in
    FA_PLUG = "<span font='FontAwesome'>\uf1e6</span>"

    fulltext = ""
    timeleft = ""

    if state == "Discharging":
        time = commasplitstatus[-1].split()[0]
        time = ":".join(time.split(":")[0:2])
        timeleft = " ({})".format(time)
    elif state == "Full" or state == "Not charging":
        fulltext = FA_PLUG + " "
    elif state == "Charging":
        fulltext = FA_PLUG + " " + FA_LIGHTNING + " "
    else:
        fulltext = "<span font='FontAwesome'>\uf128</span> "


    def color(percent, state):
        if state == "Full" or state == "Not charging":
            return "#999999"
        if percent < 10:
            # exit code 33 will turn background red
            return "#FFFFFF"
        if percent < 20:
            return "#FF3300"
        if percent < 30:
            return "#FF6600"
        if percent < 40:
            return "#FF9900"
        if percent < 50:
            return "#FFCC00"
        if percent < 60:
            return "#FFFF00"
        if percent < 70:
            return "#FFFF33"
        if percent < 80:
            return "#FFFF66"
        return "#FFFFFF"

    def battery_icon(percent):
        if percentleft <= 10:
            return '\uf244'  # empty
        elif percentleft <= 30:
            return '\uf243'  # quarter
        elif percentleft <= 60:
            return '\uf242'  # half
        elif percentleft <= 90:
            return '\uf241'  # three-quarters
        else:
            return '\uf240'  # full

    form = '<span font="FontAwesome">{}</span> <span color="{}">{}%</span>'
    fulltext += form.format(battery_icon(percentleft), color(percentleft, state), percentleft)
    fulltext += timeleft

print(fulltext)
print(fulltext)
if percentleft < 15:
    exit(33)
