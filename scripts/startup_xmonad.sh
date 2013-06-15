#!/bin/bash

# Set up an icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 5 --transparent true --alpha 0 --tint 0x333333 --height 17 &
 
# Set the background 
feh --bg-fill ~/Dropbox/backgrounds/crow.jpg

# Fire up apps
dbus-launch thunar --daemon &
dropboxd &
unclutter&
setxkbmap -layout "us, dvorak" -option "grp:caps_toggle"
 
