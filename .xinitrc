#!/bin/sh

#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#

xrandr --output DP-1 --above LVDS-1
feh --bg-fill ~/.bgimage.png
xbindkeys &
urxvtd -q -f -o &
rescuetime &
exec i3

