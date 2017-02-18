#!/bin/sh

#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#

xrandr --output DP-1 --left-of LVDS-1
feh --bg-fill ~/.bgimage.png
xbindkeys &
urxvtd -q -f -o &
exec i3

