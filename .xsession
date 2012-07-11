#!/bin/bash

# Load resources
xrdb -merge .Xresources

# Set up an icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --widthtype pixel --width 150 --tint 0x2C2E2F --transparent true --height 24 &

# Set the background color
#xsetroot -solid midnightblue

# Fire up any apps
xscreensaver -no-splash &

# Start some apps
chromium-browser & 

# Run xmonad
exec xmonad 


