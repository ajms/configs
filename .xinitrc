#!/bin/bash

source /etc/X11/xinit/xinitrc.d/50-systemd-user.sh

# set the cursor
xsetroot -cursor_name left_ptr

# xkcd-notifyd
#eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
#export SSH_AUTH_SOCK
#source /etc/X11/xinit/xinitrc.d/50-systemd-user.sh

# set up icon tray
trayer --edge top --align right --SetDockType true --SetPartialStrut true \
 --expand true --width 10 --height 18  --transparent true --tint 0x000000 --alpha 0 &

# start tray apps
dunst &
pasystray &
nm-applet &
blueman-applet &
keepassxc &
cryptomator &
pcloud &
slack &

# nightmode
redshift -l 52:13 &

# start xmonad
exec xmonad
