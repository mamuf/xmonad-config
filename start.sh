#!/bin/bash

# add following code to xmonad-start
#
## start custom scripts
#if [ -x ~/.xmonad/start.sh ] ; then
# ~/.xmonad/start.sh
#fi

xloadimage -onroot -zoom 134 ~/Dropbox/WP/wp_by_rionka.jpg &
stalonetray & 
nm-applet &
bluetooth-applet &

