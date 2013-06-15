#!/bin/bash

# Set up an icon tray
trayer_start.sh
 
# Set the background 
feh --bg-fill ~/Dropbox/backgrounds/crow.jpg

# Fire up apps
dbus-launch thunar --daemon &
dropboxd &
unclutter&
setxkbmap -layout "us, dvorak" -option "grp:caps_toggle"
 
