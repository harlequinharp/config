#!/bin/bash
ssh -f -N -L localhost:24800:allie-desk:24800 allie-desk
SYN_CMD='/usr/bin/synergyc -d FATAL --no-restart localhost'
/usr/bin/pgrep -lxf "$SYN_CMD" || ( ( $SYN_CMD ) & )
