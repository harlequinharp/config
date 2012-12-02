#!/bin/bash
 
# Load resources
xrdb -merge .Xresources
 
# Set up an icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 5 --transparent true --tint 0x191970 --height 12 &
 
# Set the background 
feh --bg-fill /home/allie/Dropbox/backgrounds/crow.jpg
 
# enable compositing
cairo-compmgr &

# Fire up apps
thunar --daemon &
pidgin &
dropboxd &
xscreensaver -no-splash &
 
# start xmonad
exec xmonad
