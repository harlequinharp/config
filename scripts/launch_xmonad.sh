#!/bin/bash

~/config/scripts/startup_xmonad.sh
# start xmonad
exec dbus-launch xmonad
