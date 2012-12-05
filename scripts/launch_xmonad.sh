#!/bin/bash
 
# Load resources
xrdb -merge .Xresources
 
# Set up an icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 5 --transparent true --alpha 0 --tint 0x0000 --height 17 &
 
# Set the background 
feh --bg-fill /home/allie/Dropbox/backgrounds/crow.jpg
 
# enable compositing
cairo-compmgr &

# Fire up apps
dbus-launch thunar --daemon &
pidgin &
dropboxd &
xscreensaver -no-splash &
 
# start xmonad
exec xmonad
