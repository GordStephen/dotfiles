#!/bin/sh

#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
#

feh --bg-fill ~/.bgimage.png
xbindkeys &
urxvtd -q -f -o &
exec i3 --force-xinerama -V >>~/.i3/i3log 2>&1

