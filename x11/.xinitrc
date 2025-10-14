#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
  xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
  xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
  xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
  xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

trayer --edge top --align right --SetDockType true --SetPartialStrut true \
  --expand true --width 10 --height 18 --transparent true --tint 0x000000 --alpha 0 &

if [ -z "$SSH_AGENT_PID" ]; then
  eval "$(ssh-agent -s)"
  ssh-add ~/.ssh/git_id_ecdsa_sk
  ssh-add ~/.ssh/git2_id_ecdsa_sk
fi

export __GLX_VENDOR_LIBRARY_NAME=mesa

# start tray apps
dunst &
pasystray &
nm-applet &
blueman-applet &
QT_SCALE_FACTOR=2 keepassxc &
mattermost-desktop &
thunderbird &
pcloud &

# nightmode
redshift -l 52:13 &

# start xmonad
exec xmonad
