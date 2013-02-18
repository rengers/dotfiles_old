#!/bin/bash

# Load resources
xrdb -merge .Xresources

# Set up an icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut false --expand true --widthtype pixel --width 200 --tint 0x2C2E2F --transparent true --height 24 &

# Setup keyboard qwerty and dvorak
setxkbmap -option "" -layout "us,us(dvp)" -option grp:alt_space_toggle -option caps:swapescape


# Set the background color
#xsetroot -solid midnightblue

# Fire up any apps
xscreensaver -no-splash &

# Start some apps
firefox & 

# Start spotify notify
./Downloads/spotify-notify/spotify-notify -s &

# gnome sound applet
gnome-sound-applet & 

# Run xmonad
ssh-agent xmonad 
