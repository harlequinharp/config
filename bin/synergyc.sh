#!/bin/bash
SYN_CMD='/usr/bin/synergyc -d FATAL --no-restart allie-desk'
/usr/bin/pgrep -lxf "$SYN_CMD" || ( ( $SYN_CMD ) & )
