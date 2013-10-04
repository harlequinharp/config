background yes
out_to_console yes
out_to_x no
# in seconds
update_interval 1

TEXT
^i(/home/allie/config/dzen2/icons/xbm/cpu.xbm) ^fg(\#FFFFFF)${cpu}% ^i(/home/allie/config/dzen2/icons/xbm/mem.xbm) ^fg(\#FFFFFF)${memperc}% ^fg(\#ebac54) ${if_existing /proc/net/route enp0s25} ^i(/home/allie/config/dzen2/icons/xbm/net_wired.xbm) ^fg(\#00aa4a) ^i(/home/allie/config/dzen2/icons/xbm/net_down_03.xbm)${downspeed enp0s25} ^fg(\#ff3333) ^i(/home/allie/config/dzen/icons/xbm/net_up_03.xbm)${upspeed enp0s25}
